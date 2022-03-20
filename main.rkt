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

; returns the first variable in the first layer of the state
(define firstVarState
  (lambda (state)
    (caaar state)))

; returns the first value in the first layer of the state
(define firstValState
  (lambda (state)
    (caadar state)))

; returns the first variable in the current layer
(define firstVarLayer
  (lambda (state)
    (caar state)))

; returns the first value of the current layer
(define firstValLayer
  (lambda (state)
    (caadr state)))

; returns box with redefined value
(define redefFirstValState
  (lambda (state value)
    (begin (set-box! (firstVarState state) value) (unbox (firstVarState state)))))

; returns boxed value
(define boxVal
  (lambda (value)
    (box value)))

; returns value from its box
(define valFromBox
  (lambda (boxVal)
    (unbox boxVal)))

; adds new layer to the state
(define addlayer
  (lambda (state)
    (cons '(() ()) state)))

; returns the next layer in the state
(define getNextLayer cadr)

; returns the state with the top layer removed
(define getNextLayers cdr)

; returns the first layer's variables
(define firstLayerVars caar)

; returns the first layer's values
(define firstLayerVals cadar)

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

; returns the variables list of the current layer
(define getVarsFromLayer car)

; returns the values list of the current layer
(define getValsFromLayer cadr)

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

; Returns the value of a variable when it's defined. This defaults to 'z if there is no value to be assigned
(define vardefine
  (lambda (expr state)
    (cond
      ((varDefined? (secondExpr expr) state) (error "Redefine Error"))
      ((null? (afterSecondExpr expr)) (boxVal 'z))
      (else (M_value (thirdExpr expr) state)))))

; Handles if the if statement has the optional else expression
(define ifstatementhandler
  (lambda (expr state break)
    (cond
      ((M_boolean (secondExpr expr) state) (M_state (thirdExpr expr) state break))
      ((null? (afterFourthExpr expr)) state)
      (else (M_state (fourthExpr expr) state break)))))

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
      ((number? (valFromBox val)) (valFromBox val))
      ((boolean? (valFromBox val)) (booleanToOutput (valFromBox val))))))

;(getUpdatedValues 'x '5 '(((a b c) (1 5 7)) ((y x z) (2 3 6))))

(define getUpdatedValues
  (lambda (var value state)
    (cond
      ((null? state) (error "Value not declared"))
      (else (firstLayerVars state) (cons (list (firstLayerVars state) (getUpdatedValuesLayer var value (firstLayer state)))
                                            (if (varDefined? var (cons (firstLayer state) '())) (getNextLayers state) (getUpdatedValues var value (getNextLayers state))))))))

(define getUpdatedValuesLayer
  (lambda (var value layer)
    (cond
      ((null? (getVarsFromLayer layer)) '())
      ((eq? var (firstVarLayer layer)) (cons (boxVal value) (cdrVals layer))) ;(()())
      (else (cons (firstValLayer layer) (getUpdatedValuesLayer var value (list (cdrVars layer) (cdrVals layer))))))))

; gets the value of a variable from the state
(define getFromState
  (lambda (var state)
    (cond
      ((null? state) (error "Value not declared"))
      ((null? (firstLayerVars state)) (getFromState var (getNextLayers state)))
      ((eq? var (firstVarState state))
       (if (eq? (valFromBox (firstValState state)) 'z)
           (error "Value not assigned")
           (valFromBox (firstValState state))))
      (else (getFromState var (cons (list (cdrVars (firstLayer state)) (cdrVals (firstLayer state))) (getNextLayers state)))))))

; checks if a variable has been defined
(define varDefined?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((null? (firstLayerVars state)) (varDefined? var (getNextLayers state)))
      ((eq? var (firstVarState state)) #t)
      (else (varDefined? var (cons (list (cdrVars (firstLayer state)) (cdrVals (firstLayer state))) (getNextLayers state)))))))      

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
      ((state? (car parsetree)) (interpret-helper (cdr parsetree) (M_state (car parsetree) state (lambda (v) v))))
      (else (error "Invalid parse tree")))))

; returns the new state given by an expression done in an existing state
(define M_state
  (lambda (expr state break)
    (cond
      ((eq? (operator expr) '=) (getUpdatedValues (secondExpr expr) (M_value (thirdExpr expr) state) state))
      ((eq? (operator expr) 'var) (cons (list (append (firstLayerVars state) (cons (secondExpr expr) '())) (append (firstLayerVals state) (cons (vardefine expr state) '()))) (getNextLayers state)))
      ((eq? (operator expr) 'if) (ifstatementhandler expr state break))
      ((eq? (operator expr) 'while) (call/cc (lambda (break) (if (M_boolean (secondExpr expr) state)
                                        (M_state expr (M_state (thirdExpr expr) state break) break)
                                        state))))
      ((eq? (operator expr) 'return) (valueReturnHandler (M_value (secondExpr expr) state)))
      ((eq? (operator expr) 'break) (break state))
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
      ((eq? (operator expr) '=) (if (number? (thirdExpr expr))
                                    (M_value (secondExpr expr) (getUpdatedValues (secondExpr expr) (M_value (thirdExpr expr) state) state))
                                    (M_value (secondExpr expr) (getUpdatedValues (secondExpr expr) (M_value (thirdExpr expr) state) state))));(M_state (thirdExpr expr) state (lambda (v) v))) state)))) ;(getUpdatedValues (secondExpr expr) (M_value (thirdExpr expr) state) state)))
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
