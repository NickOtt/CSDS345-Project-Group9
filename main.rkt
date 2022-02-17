#lang racket

(require (file "simpleParser.rkt"))

(define operator
  (lambda (expr)
    (car expr)))

(define leftop cadr)
(define rightop caddr)

; (define state '((bob steve buh brunch) (1 2 3 4)))

;'(define M_state
;  (lambda (expr state)))

(define M_value
  (lambda (expr state)
    (cond
      ((null? expr) 0)
      ((number? expr) expr)
      ;((contains? expr state) (getFromState ) ;check if variable is contained in the state and substitute with its value
      ((not (list? expr)) (getFromState expr state)) 
      ((eq? (operator expr) '+) (+ (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '-) (- (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '*) (* (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '/) (/ (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '%) (remainder (M_value (leftop expr) state) (M_value (rightop expr) state)))
      (error "Invalid value expression"))))

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
      ((eq? (operator expr) '!) (not (M_value (leftop) state)))
      ((eq? (operator expr) '==) (= (M_value (leftop) state) (M_value (rightop) state)))
      ((eq? (operator expr) '!=) (not (= (M_value (leftop) state) (M_value (rightop) state))))
      ((eq? (operator expr) '<) (< (M_value (leftop) state) (M_value (rightop) state)))
      ((eq? (operator expr) '>) (> (M_value (leftop) state) (M_value (rightop) state)))
      ((eq? (operator expr) '<=) (<= (M_value (leftop) state) (M_value (rightop) state)))
      ((eq? (operator expr) '>=) (>= (M_value (leftop) state) (M_value (rightop) state)))
      ((eq? (operator expr) '&&) (and (M_boolean (leftop) state) (M_boolean (rightop) state)))
      ((eq? (operator expr) '||) (or (M_boolean (leftop) state) (M_boolean (rightop) state)))
      (else (error "Not a boolean")))))
