#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->CPYFP/struct i)
  (list
    (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 12 16)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->CPYFP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #x0)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFP (int->CPYFP/struct i))])
)

(define (CPYFP->int rcw)
  (match-define (CPYFP sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x0 26)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift #x0 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFP (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFP->int
  #:property prop:try-from-int int->CPYFP
)

(provide (struct-out CPYFP))
