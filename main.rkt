#lang racket

(require (file "simpleParser.rkt"))

(define operator
  (lambda (expr)
    (car expr)))

(define leftop cadr)
(define rightop caddr)

;(define M_state
 ; (lambda (expr state)))

(define M_value
  (lambda (expr state)
    (cond
      ((null? expr) 0)
      ((number? expr) expr)
      ((contains? expr state) ) ;check if variable is contained in the state and substitute with its value
      ((eq? (operator expr) '+) (+ (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '-) (- (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '*) (* (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '/) (/ (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '%) (remainder (M_value (leftop expr) state) (M_value (rightop expr) state))))
      (error "Invalid value expression")))

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