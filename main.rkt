#lang racket

(require (file "simpleParser.rkt"))

;;;; ***************************************************
;;;; Jerry Chen (jmc329), Bradley Kolar (bsk61), Nicholas Ott (njo12)
;;;; EECS 345 Spring 2022
;;;; Simple Language Interpreter Project
;;;; ***************************************************

;Abstractions
; repeated calls used throughout

; gets the operator of an expression
(define operator
  (lambda (expr)
    (car expr)))

; gets the second expression
(define secondExpr cadr)

; gets the third expression
(define thirdExpr caddr)

; gets the fourth expression
(define fourthExpr cadddr)

; returns the first variable in the layer
(define firstvar
  (lambda (state)
    (caaar state)))

; returns the first value in the layer
(define firstval
  (lambda (state)
    (caaadr state)))

; adds new layer to the state
(define addlayer
  (lambda (state)
    (cons '(() ()) state)))

; returns the next layer in the state
(define getNextLayer cadr)

; returns the state with the top layer removed
(define getNextLayers cdr)

; returns the first layers variables
(define firstLayerVars caar)

; returns the first layer
(define firstLayer car)

; all values of the variable list except the first
(define cdrVars cdar)

; all value of the value list except the first
(define cdrVals cdadr)

; everything after the second expression in a list
(define afterSecondExpr cddr)

; everything after the third expression of a list
(define afterFourthExpr cdddr)

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
    (cond
      ((varDefined? (secondExpr expr) state) (error "Redefine Error"))
      ((null? (afterSecondExpr expr)) 'z)
      (else (M_value (thirdExpr expr) state)))))

; Handles if the if statement has the optional else expression
(define ifstatementhandler
  (lambda (expr state)
    (cond
      ((M_boolean (secondExpr expr) state) (M_state (thirdExpr expr) state))
      ((null? (afterFourthExpr expr)) state)
      (else (M_state (fourthExpr expr) state)))))

; Handles if the minus operator has one or two operands
(define minushandler
  (lambda (expr state)
    (if (null? (afterSecondExpr expr))
        (* -1 (M_value (secondExpr expr) state))
        (- (M_value (secondExpr expr) state) (M_value (thirdExpr expr) state)))))

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
      ((null? (firstLayerVars state)) (error "Value not declared"))
      ((eq? var (firstVar state)))
       (if (eq? (firstVal state)) 'z)
           (error "Value not assigned")
           (firstVal  state))))
      (else (getFromState var (cons (list (cdrVars state) (cdrVals state)) (getNextLayers state))))))

; checks if a variable has been defined
(define varDefined?
  (lambda (var state)
    (cond
      ((null? (firstLayer state)) #f)
      ((null? (firstLayerVars state)) (varDefined? var (removeLayer state)))
      ((eq? var (firstvar state)) #t)
      (else (varDefined? var (list (cdrVars state) (cdrVals state)))))))
      

;Main functions
; the functions that do the heavy work

; call this on a file to interpret it
(define interpret
  (lambda (filename)
    (interpret-helper (parser filename) '((() ())))))

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
      ((eq? (operator expr) '=) (list (car state) (getUpdatedValues (secondExpr expr) (M_value (thirdExpr expr) state) state)))
      ((eq? (operator expr) 'var) (list (append (car state) (cons (secondExpr expr) '())) (append (car (cdr state)) (cons (vardefine expr state) '()))))
      ((eq? (operator expr) 'if) (ifstatementhandler expr state))
      ((eq? (operator expr) 'while) (if (M_boolean (secondExpr expr) state)
                                        (M_state expr (M_state (thirdExpr expr) state))
                                        state))
      ((eq? (operator expr) 'return) (valueReturnHandler (M_value (secondExpr expr) state)))
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
      ((eq? (operator expr) '+) (+ (M_value (secondExpr expr) state) (M_value (thirdExpr expr) state)))
      ((eq? (operator expr) '-) (minushandler expr state))
      ((eq? (operator expr) '*) (* (M_value (secondExpr expr) state) (M_value (thirdExpr expr) state)))
      ((eq? (operator expr) '/) (quotient (M_value (secondExpr expr) state) (M_value (thirdExpr expr) state)))
      ((eq? (operator expr) '%) (remainder (M_value (secondExpr expr) state) (M_value (thirdExpr expr) state)))
      ((booleanOp? (operator expr)) (M_boolean expr state))
      (else (error "Invalid value expression")))))

; returns the boolean value of a given expression
(define M_boolean
  (lambda (expr state)
    (cond
      ((boolean? expr) expr)
      ((booleanVal? expr) (booleanVal expr))
      ((not (list? expr)) (getFromState expr state)) ;if the expression is a variable
      ((eq? (operator expr) '!) (not (M_value (secondExpr expr) state)))
      ((eq? (operator expr) '==) (eq? (M_value (secondExpr expr) state) (M_value (thirdExpr expr) state)))
      ((eq? (operator expr) '!=) (not (= (M_value (secondExpr expr) state) (M_value (thirdExpr expr) state))))
      ((eq? (operator expr) '<) (< (M_value (secondExpr expr) state) (M_value (thirdExpr expr) state)))
      ((eq? (operator expr) '>) (> (M_value (secondExpr expr) state) (M_value (thirdExpr expr) state)))
      ((eq? (operator expr) '<=) (<= (M_value (secondExpr expr) state) (M_value (thirdExpr expr) state)))
      ((eq? (operator expr) '>=) (>= (M_value (secondExpr expr) state) (M_value (thirdExpr expr) state)))
      ((eq? (operator expr) '&&) (and (M_boolean (secondExpr expr) state) (M_boolean (thirdExpr expr) state)))
      ((eq? (operator expr) '||) (or (M_boolean (secondExpr expr) state) (M_boolean (thirdExpr expr) state)))
      (else (error "Not a boolean")))))
