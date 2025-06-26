#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->SWP/struct i)
  (list
    (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 23 24)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->SWP i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) #x1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply SWP (int->SWP/struct i))])
)

(define (SWP->int swp)
  (match-define (SWP size a r rs rn rt) swp)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift 1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift 1 15)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct SWP (size a r rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int SWP->int
  #:property prop:try-from-int int->SWP
)

(provide (struct-out SWP))

(define int->SWPB/struct int->SWP/struct)

(define (int->SWPB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply SWPB (int->SWPB/struct i))])
)

(define (SWPB->int swp)
  (match-define (SWPB size a r rs rn rt) swp)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift 1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift 1 15)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct SWPB (size a r rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int SWPB->int
  #:property prop:try-from-int int->SWPB
)

(provide (struct-out SWPB))

(define int->SWPH/struct int->SWP/struct)

(define (int->SWPH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply SWPH (int->SWPH/struct i))])
)

(define (SWPH->int swp)
  (match-define (SWPH size a r rs rn rt) swp)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift 1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift 1 15)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct SWPH (size a r rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int SWPH->int
  #:property prop:try-from-int int->SWPH
)

(provide (struct-out SWPH))
