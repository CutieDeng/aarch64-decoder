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

(define DMB-head CLREX-head)

(define int->DMB/struct int->CLREX/struct)

(define (int->DMB i)
  (cond [(nand (equal? (bitwise-bit-field i 12 32) DMB-head)
    (equal? (bitwise-bit-field i 7 8) #x1)
    (equal? (bitwise-bit-field i 5 7) #x1)
    (equal? (bitwise-bit-field i 0 5) #x1f))
    #f]
    [else (apply DMB (int->DMB/struct i))])
)

(define (DMB->int cl)
  (match-define (DMB crm) cl)
  (bitwise-ior
    (arithmetic-shift DMB-head 12)
    (arithmetic-shift crm 8)
    (arithmetic-shift #x1 7)
    (arithmetic-shift #x1 5)
    #x1f
  )
)

(struct DMB (crm)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int DMB->int
  #:property prop:try-from-int int->DMB
)

(provide (struct-out DMB))

(define DSB-head CLREX-head)

(define int->DSB/struct int->CLREX/struct)

(define (int->DSB i)
  (cond [(nand (equal? (bitwise-bit-field i 12 32) DSB-head)
    (equal? (bitwise-bit-field i 7 8) #x1)
    (equal? (bitwise-bit-field i 5 7) #x0)
    (equal? (bitwise-bit-field i 0 5) #x1f))
    #f]
    [else (apply DSB (int->DSB/struct i))])
)

(define (DSB->int cl)
  (match-define (DSB crm) cl)
  (bitwise-ior
    (arithmetic-shift DSB-head 12)
    (arithmetic-shift crm 8)
    (arithmetic-shift #x1 7)
    #x1f
  )
)

(struct DSB (crm)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int DSB->int
  #:property prop:try-from-int int->DSB
)

(provide (struct-out DSB))

(define ISB-head CLREX-head)

(define int->ISB/struct int->CLREX/struct)

(define (int->ISB i)
  (cond [(nand (equal? (bitwise-bit-field i 12 32) ISB-head)
    (equal? (bitwise-bit-field i 7 8) #x1)
    (equal? (bitwise-bit-field i 5 7) #x2)
    (equal? (bitwise-bit-field i 0 5) #x1f))
    #f]
    [else (apply ISB (int->ISB/struct i))])
)

(define (ISB->int cl)
  (match-define (ISB crm) cl)
  (bitwise-ior
    (arithmetic-shift ISB-head 12)
    (arithmetic-shift crm 8)
    (arithmetic-shift #x1 7)
    (arithmetic-shift #x2 5)
    #x1f
  )
)

(struct ISB (crm)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ISB->int
  #:property prop:try-from-int int->ISB
)

(provide (struct-out ISB))
