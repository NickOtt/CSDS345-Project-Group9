#lang racket

(require (file "simpleParser.rkt"))

(define M_state
  (lambda (expr state)))

(define M_value
  (lambda (expr state)
    (cond
      ((null? expr) 0)
      ((number? expr) expr)
      ((contains? expr state) ) ;check if variable is contained in the state and substitute with its value
      ((eq? (car expr) '%) (remainder (M_value (cadr expr) state) (M_value (caddr expr) state))))
      ((eq? (car expr) '*) (* (M_value (cadr expr) state) (M_value (caddr expr) state)))
      ((eq? (car expr) '/) (/ (M_value (cadr expr) state) (M_value (caddr expr) state)))
      ((eq? (car expr) '+) (+ (M_value (cadr expr) state) (M_value (caddr expr) state)))
      ((eq? (car expr) '-) (- (M_value (cadr expr) state) (M_value (caddr expr) state)))
      (error "Invalid value expression")))

(define M_boolean
  (lambda (expr state)))