#lang racket

(require (file "simpleParser.rkt"))

<<<<<<< HEAD
;(define M_state
;  (lambda (expr state)))
; example state:  (define state '((bob steve buh brunch) (1 2 3 4)))
=======
(define operator
  (lambda (expr)
    (car expr)))

(define leftop cadr)
(define rightop caddr)

(define M_state
  (lambda (expr state)))
>>>>>>> mvalue-Nick

(define M_value
  (lambda (expr state)
    (cond
      ((null? expr) 0)
      ((number? expr) expr)
<<<<<<< HEAD
      ;((contains? expr state) (getFromState ) ;check if variable is contained in the state and substitute with its value
      ((not (list? expr)) (getFromState expr state)) 
      ((eq? (car expr) '%) (remainder (M_value (cadr expr) state) (M_value (caddr expr) state)))
      ((eq? (car expr) '*) (* (M_value (cadr expr) state) (M_value (caddr expr) state)))
      ((eq? (car expr) '/) (/ (M_value (cadr expr) state) (M_value (caddr expr) state)))
      ((eq? (car expr) '+) (+ (M_value (cadr expr) state) (M_value (caddr expr) state)))
      ((eq? (car expr) '-) (- (M_value (cadr expr) state) (M_value (caddr expr) state)))
      (else (error "Invalid operator")))))

(define getFromState
  (lambda (var state)
    (cond
      ((eq? var (caar state))
       (if (eq? (cadr state) 'z) (error "Value not declared") (caadr state)))
      ((null? (cdar state)) (error "Value not initialized"))
      (else (getFromState var (list (cdar state) (cdadr state)))))))
=======
      ((contains? expr state) ) ;check if variable is contained in the state and substitute with its value
      ((eq? (operator expr) '+) (+ (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '-) (- (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '*) (* (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '/) (/ (M_value (leftop expr) state) (M_value (rightop expr) state)))
      ((eq? (operator expr) '%) (remainder (M_value (leftop expr) state) (M_value (rightop expr) state))))
      (error "Invalid value expression")))
>>>>>>> mvalue-Nick

(define M_boolean
  (lambda (expr state)
    (cond
      ((null? expr) 
    ))))