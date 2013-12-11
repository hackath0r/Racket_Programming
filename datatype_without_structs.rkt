#lang racket 

(provide (all-defined-out))

; Note: arguably bad style to not have else clause
(define (funny-sum xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (funny-sum (cdr xs)))]
        [(string? (car xs)) (+ (string-length (car xs))
                               (funny-sum (cdr xs)))]))


; now implement same idea as 
; datatype exp = Cons of int | Negate of exp
;              | Add of exp * exp | Multiply of exp *exp

; just helper fuctions that make lists where first element
; is a symbol
(define (Const i) (list 'Const i))
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2))
(define (Multiply e1 e2) (list 'Multiply e1 e2))

; just helper functions that test what "kind of exp"
; Note: More robust could raise better errors of non-exp values
(define (Const? x) (eq? (car x) 'Const))
(define (Negate? x) (eq? (car x) 'Negate))
(define (Add? x) (eq? (car x) 'Add))
(define (Multiply? x) (eq? (car x) 'Multiply))

; just helper functions that get the pieces for "one kind of exp"
; Note: More robust could check "what kind of exp"
(define (Const-int e) (car (cdr e)))
(define (Negate-e e) (car (cdr e)))
(define (Add-e1 e) (car (cdr e)))
(define (Add-e2 e) (car (cdr (cdr e))))
(define (Multiply-e1 e) (car (cdr (cdr e))))
(define (Multiply-e2 e) (car (cdr (cdr e))))

; same recursive structure as we have in ML, just without pattern matching
; end one change from what we did before: returning an exp, in particularly
; a Constant, rather than an int
(define (eval-exp e)
  (cond [(Const? e) e]; not returning an exp, not a int
        [(Negate? e) (Const (- (Const-int (eval-exp (Negate-e e)))))]
        [(Add? e) (let ([v1 (Const-int (eval-exp (Add-e1 e)))]
                        [v2 (Const-int (eval-exp (Add-e2 e)))])
                    (Const (+ v1 v2)))]
        [(Multiply? e) (let ([v1 (Const-int (eval-exp (Multiply-e1 e)))]
                             [v2 (Const-int (eval-exp (Multiply-e2 e)))])
                         (Const (* v1 v2)))]
        [#t (error "eval-exp expected an exp")]))
        