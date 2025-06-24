#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LDAXR/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 10 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDAXR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 24 30) #x8)
    (equal? (bitwise-bit-field i 23 24) 0)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply LDAXR (int->LDAXR/struct i))])
)

(define (LDAXR->int l)
  (match-define (LDAXR size l rs o0 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x8 24)
    (arithmetic-shift l 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAXR (size l rs o0 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDAXR->int
  #:property prop:try-from-int int->LDAXR
)

(provide (struct-out LDAXR))
