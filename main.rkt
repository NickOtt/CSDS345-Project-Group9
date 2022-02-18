#lang racket

(require (file "simpleParser.rkt"))

;Abstractions
; repeated calls used throughout

; gets the operator of an expression
(define operator
  (lambda (expr)
    (car expr)))

; gets the left operand of the expression
(define leftop cadr)

; gets the right operand of the expression
(define rightop caddr)

; gets the second right operand of the expression
(define rightrightop cadddr)

; returns the first variable in the state
(define firstvar
  (lambda (state)
    (caar state)))

; returns the first value in the state
(define firstval
  (lambda (state)
    (caadr state)))

; all values of the variable list except the first
(define cdrVars cdar)

; all value of the value list except the first
(define cdrVals cdadr)

; the third expression in a list
(define thirdExpr cddr)

; the forth expression of a list
(define forthExpr cdddr)

;Helper Functions
; functions that are used to assist other methods

; returns true if the expression starts with a state operator
(define state?
  (lambda (expr)
    (cond
      ((eq? (operator expr) '=) #t)
      ((eq? (operator expr) 'var) #t)
      ((eq? (operator expr) 'if) #t)
      ((eq? (operator expr) 'while) #t)
      ((eq? (operator expr) 'return) #t)
      (else #f))))

; checks if the expression is a boolean value
(define booleanVal?
  (lambda (expr)
    (cond
      ((or (eq? expr 'true) (eq? expr 'false)) #t)
      (else #f))))

; changes from 'true/'false to #t/#f
(define booleanVal
  (lambda (bool)
    (cond
      ((eq? bool 'true) #t)
      ((eq? bool 'false) #f)
      (else (error "Invalid boolean")))))

; changes from #t/#f to 'true/'false
(define booleanToOutput
  (lambda (val)
    (cond
      (val 'true)
      ((not val) 'false))))

; returns true if the operator is a boolean operator
(define booleanOp?
  (lambda (op)
    (cond
      ((eq? op '!) #t)
      ((eq? op '==) #t)
      ((eq? op '!=) #t)
      ((eq? op '<) #t)
      ((eq? op '>) #t)
      ((eq? op '<=) #t)
      ((eq? op '>=) #t)
      ((eq? op '&&) #t)
      ((eq? op '||) #t)
      (else #f))))

; Assigns a variable its value. This defaults to 'z if there is no value assigned
(define vardefine
  (lambda (expr state)
    (if (null? (thirdExpr expr))
        'z
        (M_value (rightop expr) state))))

; Handles if the if statement has the optional else expression
(define ifstatementhandler
  (lambda (expr state)
    (cond
      ((M_boolean (leftop expr) state) (M_state (rightop expr) state))
      (else (if (null? (forthExpr expr))
                state
                (M_state (rightrightop expr) state))))))

; Handles if the minus operator has one or two operands
(define minushandler
  (lambda (expr state)
    (if (null? (thirdExpr expr))
        (* -1 (M_value (leftop expr) state))
        (- (M_value (leftop expr) state) (M_value (rightop expr) state)))))

; Handles converting a value to its proper return type if needed
(define valueReturnHandler
  (lambda (val)
    (cond
      ((number? val) val)
      ((boolean? val) (booleanToOutput val)))))

; Gets the updated value of a variable
(define getUpdatedValues
  (lambda (var value state)
    (cond
      ((not (pair? (car state))) (error "Value not declared"))
      ((eq? var (firstvar state)) (cons value (cdadr state)))
      (else (cons (caadr state) (getUpdatedValues var value (list (cdrVars state) (cdrVals state))))))))

; gets the value of a variable from the state
(define getFromState
  (lambda (var state)
    (cond
      ((not (pair? (car state))) (error "Value not declared"))
      ((eq? var (firstvar state))
       (if (eq? (firstval state) 'z)
           (error "Value not assigned")
           (firstval state)))
      (else (getFromState var (list (cdrVars state) (cdrVals state)))))))

;Main functions
; the functions that do the heavy work

; call this on a file to interpret it
(define interpret
  (lambda (filename)
    (interpret-helper (parser filename) '(() ()))))

; processes the parsetree and return the value of the tree
(define interpret-helper
  (lambda (parsetree state)
    (cond
      ((not (list? state)) state)
      ((state? (car parsetree)) (interpret-helper (cdr parsetree) (M_state (car parsetree) state)))
      (else (error "Invalid parse tree")))))
        
; returns the new state given by an expression done in an existing state
(define M_state
  (lambda (expr state)
    (cond
      ((eq? (operator expr) '=) (list (car state) (getUpdatedValues (leftop expr) (M_value (rightop expr) state) state)))
      ((eq? (operator expr) 'var) (list (append (car state) (cons (leftop expr) '())) (append (car (cdr state)) (cons (vardefine expr state) '()))))
      ((eq? (operator expr) 'if) (ifstatementhandler expr state))
      ((eq? (operator expr) 'while) (if (M_boolean (leftop expr) state)
                                        (M_state expr (M_state (rightop expr) state))
                                        state))
      ((eq? (operator expr) 'return) (valueReturnHandler (M_value (leftop expr) state)))
      (else (error "Invalid state expression")
    ))))

; returns the value of a given expression. This could be an integer or a boolean
(define M_value
  (lambda (expr state)
    (cond
      ((null? expr) 'z)
      ((number? expr) expr)
      ((booleanVal? expr) (booleanVal expr))
      ((not (list? expr)) (getFromState expr state)) ;if the expression is a variable
      ((eq? (operator expr) '+) (+ (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '-) (minushandler expr state))
      ((eq? (operator expr) '*) (* (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '/) (quotient (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '%) (remainder (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((booleanOp? (operator expr)) (M_boolean expr state))
      (else (error "Invalid value expression")))))

; returns the boolean value of a given expression
(define M_boolean
  (lambda (expr state)
    (cond
      ((boolean? expr) expr)
      ((booleanVal? expr) (booleanVal expr))
      ((not (list? expr)) (getFromState expr state)) ;if the expression is a variable
      ((eq? (operator expr) '!) (not (M_value (leftop expr) state)))
      ((eq? (operator expr) '==) (eq? (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '!=) (not (= (M_value (leftop expr) state) (M_value (rightop expr) state))))
      ((eq? (operator expr) '<) (< (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '>) (> (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '<=) (<= (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '>=) (>= (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '&&) (and (M_boolean (leftop expr) state) (M_boolean (rightop expr) state)))
      ((eq? (operator expr) '||) (or (M_boolean (leftop expr) state) (M_boolean (rightop expr) state)))
      (else (error "Not a boolean")))))
