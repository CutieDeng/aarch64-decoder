#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->FMSUB/struct i)
  (list
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 21 22)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 10 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->FMSUB i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 29 30) #x0)
    (equal? (bitwise-bit-field i 24 29) #x1f)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 15 16) #x1)
  ) #f]
  [else (apply FMSUB (int->FMSUB/struct i))])
)

(define (FMSUB->int f)
  (match-define (FMSUB ftype o1 rm o0 ra rn rd) f)
  (bitwise-ior
    (arithmetic-shift #x1f 24)
    (arithmetic-shift ftype 22)
    (arithmetic-shift o1 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift ra 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct FMSUB (ftype o1 rm o0 ra rn rd)
  #:transparent
  #:property prop:in-feature #f
  #:property prop:into-int FMSUB->int
  #:property prop:try-from-int int->FMSUB
)

(provide (struct-out FMSUB))

(define (int->FMUL/e/struct i)
  (list
    (bitwise-bit-field i 29 30)
    (bitwise-bit-field i 21 22)
    (bitwise-bit-field i 20 21)
    (bitwise-bit-field i 16 20)
    (bitwise-bit-field i 11 12)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->FMUL/e i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 30 32) #x1)
    (equal? (bitwise-bit-field i 29 30) #x0)
    (equal? (bitwise-bit-field i 24 29) #x1f)
    (equal? (bitwise-bit-field i 22 24) #x0)
    (equal? (bitwise-bit-field i 12 16) #x9)
    (equal? (bitwise-bit-field i 10 11) #x0)
  ) #f]
  [else (apply FMUL/e (int->FMUL/e/struct i))])
)

(define (FMUL/e->int f)
  (match-define (FMUL/e u l m rm h rn rd) f)
  (bitwise-ior
    (arithmetic-shift #x1 30)
    (arithmetic-shift u 29)
    (arithmetic-shift #x1f 24)
    (arithmetic-shift #x0 22)
    (arithmetic-shift l 21)
    (arithmetic-shift m 20)
    (arithmetic-shift rm 16)
    (arithmetic-shift #x9 12)
    (arithmetic-shift h 11)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct FMUL/e (u l m rm h rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_FP16
  #:property prop:into-int FMUL/e->int
  #:property prop:try-from-int int->FMUL/e
)

(provide (struct-out FMUL/e))
