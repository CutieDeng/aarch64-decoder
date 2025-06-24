#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LDAPR/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDAPR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) #x4)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDAPR (int->LDAPR/struct i))])
)

(define (LDAPR->int l)
  (match-define (LDAPR size rs rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift #x1 23)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift #x1 15)
    (arithmetic-shift #x4 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAPR (size rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC . #t))
  #:property prop:into-int LDAPR->int
  #:property prop:try-from-int int->LDAPR
)

(provide (struct-out LDAPR))

(define int->LDAPRB/struct int->LDAPR/struct)

(define (int->LDAPRB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) #x4)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDAPRB (int->LDAPRB/struct i))])
)

(define (LDAPRB->int l)
  (match-define (LDAPRB size rs rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift #x1 23)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift #x1 15)
    (arithmetic-shift #x4 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAPRB (size rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC . #t))
  #:property prop:into-int LDAPRB->int
  #:property prop:try-from-int int->LDAPRB
)

(provide (struct-out LDAPRB))
