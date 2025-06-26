#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LD64B/struct i)
  (list
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LD64B i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) #x3)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 22) 0)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) #x5)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LD64B (int->LD64B/struct i))])
)

(define (LD64B->int ldadd)
  (match-define (LD64B rn rt) ldadd)
  (bitwise-ior
    (arithmetic-shift #x3 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift 1 21)
    (arithmetic-shift #x1f 16)
    (arithmetic-shift 1 15)
    (arithmetic-shift #x5 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LD64B (rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LS64 . #t))
  #:property prop:into-int LD64B->int
  #:property prop:try-from-int int->LD64B
)

(provide (struct-out LD64B))

(define int->ST64B/struct int->LD64B/struct)

(define (int->ST64B i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) #x3)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 22) 0)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) #x1)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply ST64B (int->ST64B/struct i))])
)

(define (ST64B->int ldadd)
  (match-define (ST64B rn rt) ldadd)
  (bitwise-ior
    (arithmetic-shift #x3 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift 1 21)
    (arithmetic-shift #x1f 16)
    (arithmetic-shift 1 15)
    (arithmetic-shift #x1 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST64B (rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LS64 . #t))
  #:property prop:into-int ST64B->int
  #:property prop:try-from-int int->ST64B
)

(provide (struct-out ST64B))
