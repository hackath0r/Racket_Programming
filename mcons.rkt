#lang racket

(provide (all-defined-out))

; use of mcons, mcar, mcdr, mpair?, set-mcar!, set-mcdr!
; the only difference between cons and mocons is the using 
; mcons allows to mutate the pair while cons doesn't

(define mpr (mcons 4 (mcons #t "hi")))

