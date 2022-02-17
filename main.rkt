#lang racket

(require (file "simpleParser.rkt"))

(define operator
  (lambda (expr)
    (car expr)))

(define leftop cadr)
(define rightop caddr)
(define rightrightop cadddr)

(define state?
  (lambda (expr)
    (cond
      ((eq? (operator expr) '=) #t)
      ((eq? (operator expr) 'var) #t)
      ((eq? (operator expr) 'if) #t)
      ((eq? (operator expr) 'while) #t)
      ((eq? (operator expr) 'return) #t)
      (else #f))))

(define booleanVal?
  (lambda (expr)
    (cond
      ((or (eq? expr 'true) (eq? expr 'false)) #t)
      (else #f))))

(define booleanVal
  (lambda (bool)
    (cond
      ((eq? bool 'true) #t)
      ((eq? bool 'false) #f)
      (else (error "Invalid boolean")))))

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

(define interpret
  (lambda (filename)
    (interpret-helper (parser filename) '(() ()))))

(define interpret-helper
  (lambda (parsetree state)
    (cond
      ((eq? (caar parsetree) 'return) (M_state (car parsetree) state))
      ((state? (caar parsetree)) (interpret-helper ((cdr parsetree) (M_state (car parsetree) state))))
      (else (error "Invalid parse tree")))))

; (define state '((bob steve buh brunch) (1 2 3 4)))

(define M_state
  (lambda (expr state)
    (cond
      ((eq? (operator expr) '=) (list (car state) (getUpdatedValues (leftop expr) (M_value (rightop expr) state) state)))
      ((eq? (operator expr) 'var) (list (append (car state) (cons (leftop expr) '())) (append (car (cdr state)) (cons 'z '()))))
      ((eq? (operator expr) 'if) (if (M_boolean (leftop expr) state) (M_state (rightop expr) state) (M_state (rightop expr) state)))
      ((eq? (operator expr) 'while) (if (M_boolean (leftop expr) state) (M_state expr (M_state (rightop expr) state)) (M_state (rightrightop expr) state)))
      ((eq? (operator expr) 'return) (M_value (leftop expr) state))
      (else (error "Invalid state expression")
    ))))

(define getUpdatedValues
  (lambda (var value state)
    (cond
      ((eq? var (firstvar state)) (cons value (cdadr state)))
      ((not (list? (car state))) (error "Value not initialized"))
      (else (cons (caadr state) (getUpdatedValues var value (list (cdar state) (cdadr state))))))))

(define M_value
  (lambda (expr state)
    (cond
      ((null? expr) 0)
      ((number? expr) expr)
      ((booleanVal? expr) (booleanVal expr))
      ;((contains? expr state) (getFromState ) ;check if variable is contained in the state and substitute with its value
      ((not (list? expr)) (getFromState expr state))
      ((eq? (operator expr) '+) (+ (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '-) (- (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '*) (* (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '/) (/ (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '%) (remainder (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((booleanOp? (operator expr)) (M_boolean expr state))
      (else error "Invalid value expression"))))

(define firstvar
  (lambda (state)
    (caar state)))

(define firstval
  (lambda (state)
    (caadr state)))

(define getFromState
  (lambda (var state)
    (cond
      ((eq? var (firstvar state))
       (if (eq? (firstval state) 'z) (error "Value not declared") (firstval state)))
      ((not (list? (car state))) (error "Value not initialized"))
      (else (getFromState var (list (cdar state) (cdadr state)))))))

(define M_boolean
  (lambda (expr state)
    (cond
      ((boolean? expr) expr)
      ((eq? (operator expr) '!) (not (M_value (leftop expr) state)))
      ((eq? (operator expr) '==) (= (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '!=) (not (= (M_value (leftop expr) state) (M_value (rightop expr) state))))
      ((eq? (operator expr) '<) (< (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '>) (> (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '<=) (<= (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '>=) (>= (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '&&) (and (M_boolean (leftop expr) state) (M_boolean (rightop expr) state)))
      ((eq? (operator expr) '||) (or (M_boolean (leftop expr) state) (M_boolean (rightop expr) state)))
      (else (error "Not a boolean")))))
