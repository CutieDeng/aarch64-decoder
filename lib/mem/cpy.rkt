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
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
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

(define int->CPYFPN/struct int->CPYFP/struct)

(define (int->CPYFPN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #xc)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPN (int->CPYFPN/struct i))])
)

(define (CPYFPN->int rcw)
  (match-define (CPYFPN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPN->int
  #:property prop:try-from-int int->CPYFPN
)

(provide (struct-out CPYFPN))
