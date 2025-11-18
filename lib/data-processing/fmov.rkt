#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->FMOV/r/struct i)
  (list
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 15 17)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->FMOV/r i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 29 30) #x0)
    (equal? (bitwise-bit-field i 24 29) #x1e)
    (equal? (bitwise-bit-field i 21 22) #x1)
    (equal? (bitwise-bit-field i 17 21) #x0)
    (equal? (bitwise-bit-field i 15 17) #x0)
    (equal? (bitwise-bit-field i 10 15) #x10)
  ) #f]
  [else (apply FMOV/r (int->FMOV/r/struct i))])
)

(define (FMOV/r->int f)
  (match-define (FMOV/r ftype opc rn rd) f)
  (bitwise-ior
    (arithmetic-shift #x1e 24)
    (arithmetic-shift ftype 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift opc 15)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct FMOV/r (ftype opc rn rd)
  #:transparent
  #:property prop:in-feature #f
  #:property prop:into-int FMOV/r->int
  #:property prop:try-from-int int->FMOV/r
)

(provide (struct-out FMOV/r))
