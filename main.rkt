#lang racket

require "simpleParser.rkt"

(define M_state
  (lambda (expr state)))

(define M_value
  (lambda (expr state)))

(define M_boolean
  (lambda (expr state)))