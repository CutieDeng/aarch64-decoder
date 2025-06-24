#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LDIAPP/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 10 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDIAPP i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 23 26) #x2)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 13 16) 0)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply LDIAPP (int->LDIAPP/struct i))])
)

(define (LDIAPP->int l)
  (match-define (LDIAPP size l rt2 opc2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x2 23)
    (arithmetic-shift l 22)
    (arithmetic-shift rt2 16)
    (arithmetic-shift opc2 12)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDIAPP (size l rt2 opc2 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC3 . #t))
  #:property prop:into-int LDIAPP->int
  #:property prop:try-from-int int->LDIAPP
)

(provide (struct-out LDIAPP))

(define (int->STILP/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 12 16)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STILP i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 23 26) #x2)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 13 16) 0)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply STILP (int->STILP/struct i))])
)

(define (STILP->int l)
  (match-define (STILP size l rt2 opc2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x2 23)
    (arithmetic-shift l 22)
    (arithmetic-shift rt2 16)
    (arithmetic-shift opc2 12)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STILP (size l rt2 opc2 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC3 . #t))
  #:property prop:into-int STILP->int
  #:property prop:try-from-int int->STILP
)

(provide (struct-out STILP))
