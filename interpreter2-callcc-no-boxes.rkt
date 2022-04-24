;;;; ***************************************************
;;;; Jerry Chen (jmc329) Nicholas Ott (njo12) Bradley Kolar (bsk61)
;;;; EECS 345 Spring 2022
;;;; Imperitive Language Project
;;;; ***************************************************

; If you are using scheme instead of racket, comment these two lines, uncomment the (load "simpleParser.scm") and comment the (require "simpleParser.rkt")
#lang racket
(require "classParser.rkt")
; (load "simpleParser.scm")


; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file class-name)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-main class-name
                        (create-global-environment (parser file)
                                           (newenvironment)
                                           return
                                           (lambda (env) (myerror "Break used outside of loop"))
                                           (lambda (env) (myerror "Continue used outside of loop"))
                                           (lambda (v env) (myerror "Uncaught exception thrown")))
                        (newenvironment)
                        return (lambda (env) (myerror "Break used outside of loop"))
                        (lambda (env) (myerror "Continue used outside of loop"))
                        (lambda (v env) (myerror "Uncaught exception thrown"))))))))

(define create-global-environment
  (lambda (statement-list environment-global return break continue throw)
    (cond
      ((null? statement-list) environment-global)
      (else (create-global-environment (cdr statement-list)
                              (insert
                               (class-definition-name statement-list)
                               (make-class-closure (class-extends-body statement-list) empty-class-closure environment-global)
                               environment-global)
                              return break continue throw)))))

(define make-class-closure
  (lambda (class-definition closure environment-global)
    (cond
      ((null? class-definition) closure)
      ((null? (car class-definition)) (make-class-closure (cadr class-definition) closure environment-global))
      ((eq? (class-definition-type class-definition) 'extends)
       (make-class-closure (cadr class-definition) (list
                                                   (cons (super-class-name class-definition) '())
                                                   (super-methods (lookup (super-class-name class-definition) environment-global))
                                                   (super-fields (lookup (super-class-name class-definition) environment-global)))) environment-global)
      ((or (eq? (class-definition-type class-definition) 'static-function)
           (eq? (class-definition-type class-definition) 'function))
       (make-class-closure (cdr class-definition) (list
                                                   (class-closure-super closure)
                                                   (add-to-class-closure (class-function-name class-definition) (make-closure (class-function-params (class-function class-definition)) (class-function-body (class-function class-definition)) environment-global) (class-closure-functions closure))
                                                   (class-closure-instances closure))
                           environment-global))
      ((eq? (class-definition-type class-definition) 'var)
       (make-class-closure (cdr class-definition) (list
                                                   (class-closure-super closure)
                                                   (class-closure-functions closure)
                                                   (add-to-class-closure (class-variable-name class-definition) (get-var-value (car class-definition)) (class-closure-instances closure)))
                           environment-global))
      (else myerror "invalid class body"))))

; Add a new variable/value or function/body pair to the class closure.
(define add-to-class-closure
  (lambda (var val class-closure)
    (if (exists-in-list? var (variables class-closure))
        (myerror "error: variable is being re-declared:" var)
        (list (cons var (variables class-closure)) (cons (box (scheme->language val)) (store class-closure))))))

;(((A) (#&(() ((main) (#&(() ((var a (new A)) (return (+ (dot a x) (dot a y)))) ((() ()))))) ((y x) (#&10 #&5))))))
(define interpret-main
  (lambda (class-name environment-global environment-local return break continue throw)
    (interpret-statement-list
     (get-main-body (lookup-in-frame 'main (get-class-function-list (lookup (string->symbol class-name) environment-global))))
     environment-global
     environment-local
     return break continue throw)))

(define super-fields caddr)
(define super-methods cadr)
(define class-definition-type caar)
(define empty-class-closure '(() (() ()) (() ())))
(define class-closure-super car)
(define class-closure-functions cadr)
(define class-closure-instances caddr)
(define class-extends-body cddar)
(define class-variable-name cadar)
(define class-function-name cadar)
(define class-definition-name cadar)
(define super-class-name cadar)
(define function-type cadar)
(define main-body cdddar)

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment-global environment-local return break continue throw)
    (if (null? statement-list)
        environment-local
        (interpret-statement-list (cdr statement-list)
                                  environment-global
                                  (interpret-statement (car statement-list) environment-global environment-local return break continue throw)
                                  return break continue throw))))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment-global environment-local return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment-global environment-local return throw))
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment-global environment-local throw))
      ((eq? '= (statement-type statement)) (interpret-assign statement environment-global environment-local throw))
      ((eq? 'if (statement-type statement)) (interpret-if statement environment-global environment-local return break continue throw))
      ((eq? 'while (statement-type statement)) (interpret-while statement environment-global environment-local return throw))
      ((eq? 'continue (statement-type statement)) (continue environment-local))
      ((eq? 'break (statement-type statement)) (break environment-local))
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment-global environment-local return break continue throw))
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment-global environment-local throw))
      ((eq? 'try (statement-type statement)) (interpret-try statement environment-global environment-local return break continue throw))
      ((eq? 'function (statement-type statement)) (interpret-function statement environment-global environment-local return break continue throw))
      ((eq? 'funcall (statement-type statement)) (interpret-funcall statement environment-global environment-local return break continue throw))
;      ((eq? 'class (statement-type statement)) (interpret-class statement environment return break continue throw))
      (else (myerror "Unknown statement:" (statement-type statement))))))


; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment-global environment-local return throw)
    (return (eval-expression (get-expr statement) environment-global environment-local throw))))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment-global environment-local throw)
    (if (exists-declare-value? statement)
        (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment-global environment-local throw) environment-local)
        (insert (get-declare-var statement) 'novalue environment-local))))

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment-global environment-local throw)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment-global environment-local throw) environment-global environment-local)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment-global environment-local return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment-global environment-local throw) (interpret-statement (get-then statement) environment-global environment-local return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment-global environment-local return break continue throw))
      (else environment-local))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment-global environment-local return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment-local)
                        (if (eval-expression condition environment-global environment-local throw)
                            (loop condition body (interpret-statement body environment-global environment-local return break (lambda (env) (break (loop condition body env))) throw))
                         environment-local))))
         (loop (get-condition statement) (get-body statement) environment-local))))))

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment-global environment-local return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         environment-global
                                         (push-frame environment-local)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment-global environment-local throw)
    (throw (eval-expression (get-expr statement) environment-global environment-local throw) environment-local)))
;
(define interpret-function
  (lambda (statement environment-global environment-local return break continue throw)
    (insert (operand1 statement) (make-closure (operand2 statement) (operand3 statement) environment-global environment-local) environment-local)))

(define interpret-funcall
  (lambda (statement environment-global environment-local return break continue throw)
    (interpret-statement-list (body (closure statement environment-global environment-local))
                              (bind-parameters (formalparams (closure statement environment-local)) (params statement) (push-frame (append (closure-state (closure statement environment-local)) environment-local)) environment-local throw)
                              return
                              (lambda (s) (myerror "break used outside of loop"))
                              (lambda (s) (myerror "no return statemnet"))
                              throw)))

; Interpret file with main() method and functions

;Run through global variables and function and add to state before calling main()
;(define create-base-layer
;  (lambda (statement-list environment return break continue throw)
;    (cond
;      ((null? statement-list) environment)
;      ((eq? 'main (operand1 (car statement-list))) (create-base-layer (cdr statement-list) environment return break continue throw))
;      (else (create-base-layer (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw)))))

; Makes the closure for a function
(define make-closure
  (lambda (formal-params body func-def-state)
    (list formal-params body func-def-state)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment-global environment-local return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block environment-global env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     environment-global
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement)
                                                 environment-global
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment-global environment-local return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment-global environment-local return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block environment-global env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block environment-global env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment-global environment-local return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment-global environment-local new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

; Evaluates all possible boolean and arithmetic expressions and function calls, including constants and variables.
(define eval-expression
  (lambda (expr environment-global environment-local throw)
    (cond
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((not (list? expr)) (lookup expr environment-local))
      ((eq? (car expr) 'funcall) (eval-function expr environment-global environment-local throw))
      ((eq? (car expr) 'new) (eval-new expr environment-global environment-local throw))
      (else (eval-operator expr environment-global environment-local throw)))))

(define eval-function
  (lambda (expr environment-global environment-local throw)
    (call/cc (lambda (return) (interpret-statement-list (body (closure expr environment-global environment-local))
                         (bind-parameters (formalparams (closure expr environment-local)) (params expr) (push-frame (append (closure-state (closure expr environment-local)) environment-local)) environment-local throw)
                         return
                         (lambda (s) (myerror "break used outside of loop"))
                         (lambda (s) (myerror "no return statemnet"))
                         throw)))))

(define eval-new
  (lambda (expr environment-global environment-local throw)
    (list (lookup (operand1 expr) environment-global) (create-initial-values (initial-field-values-expressions (lookup (operand1 expr) environment-global)) environment-global '((() ())) throw))))

(define create-initial-values
  (lambda (expressions environment-global environment-local throw)
    (if (null? expressions)
        '()
        (let ([current-eval (eval-expression (car expressions) environment-global environment-local throw)]) (cons (create-initial-values (cdr expressions) environment-global (insert current-eval environment-local)) (current-eval)))))) 


(define initial-field-values-expressions
  (lambda (lis)
    (cadr (caddr lis))))
    

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment-global environment-local throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment-global environment-local throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment-global environment-local throw)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment-global environment-local throw) environment-global environment-local throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment-global environment-local throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment-global environment-local throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment-global environment-local throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment-global environment-local throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment-global environment-local throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment-global environment-local throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment-global environment-local throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment-global environment-local throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment-global environment-local throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment-global environment-local throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment-global environment-local throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment-global environment-local throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment-global environment-local throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment-global environment-local throw)))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)
(define get-var-value operand2)
(define get-function-body operand2)
(define get-class-function-list operand1)
(define class-function operator)
(define class-function-params operand2)
(define class-function-body operand3)
(define get-main-body operand1)
    
(define bind-parameters
  (lambda (params args fstate state throw)
    (cond
      ((null? params) (if (null? args) fstate (throw (myerror "incorrect # of args"))))
      ((null? args) (throw (myerror "incorrect # of args")))
      (else (bind-parameters (cdr params) (cdr args) (insert (car params) (eval-expression (car args) state throw) fstate) state throw)))))



; some abstractions
(define topframe car)
(define formalparams car)
(define remainingframes cdr)
(define body cadr)
(define params cddr)
(define closure-state caddr)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))

(define closure
  (lambda (expr environment)
    (lookup (operand1 expr) environment)))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (unbox (car l)))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (box (scheme->language val)) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (box (scheme->language val)) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

