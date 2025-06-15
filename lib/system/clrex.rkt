#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define CLREX-head #xd5033)

(define (int->CLREX/struct i)
  (list (bitwise-bit-field i 8 12))
)

(define (int->CLREX i)
  (cond [(nand (equal? (bitwise-bit-field i 12 32) CLREX-head)
    (equal? (bitwise-bit-field i 5 8) #x2)
    (equal? (bitwise-bit-field i 0 5) #x1f))
    #f]
    [else (apply CLREX (int->CLREX/struct i))])
)

(define (CLREX->int cl)
  (match-define (CLREX crm) cl)
  (bitwise-ior
    (arithmetic-shift CLREX-head 12)
    (arithmetic-shift crm 8)
    (arithmetic-shift #x2 5)
    #x1f
  )
)

(struct CLREX (crm)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int CLREX->int
  #:property prop:try-from-int int->CLREX
)

(provide (struct-out CLREX))