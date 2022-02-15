#lang racket

require "simpleParser.rkt"

(define M_state
  (lambda (expr state)))

(define M_value
  (lambda (expr state)
    (cond
      ((null? expr) 0)
      ((number? expr) expr)
      ((eq? (car expr) '%) )
      ((eq? (car expr) '*) )
      ((eq? (car expr) '/) )
      ((eq? (car expr) '+) )
      ((eq? (car expr) '-) )
      (else error "Invalid value expression"))))

(define M_boolean
  (lambda (expr state)))