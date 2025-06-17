#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define ERET-literal (bitwise-ior
  (arithmetic-shift #x6b 25)
  (arithmetic-shift #x4 21)
  (arithmetic-shift #x1f 16)
  (arithmetic-shift #x1f 5)
))

(define (int->ERET i)
  (cond [(nand (equal? i ERET-literal)) #f]
    [else (ERET)])
)

(define (ERET->int _e)
  ERET-literal
)

(struct ERET ()
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ERET->int
  #:property prop:try-from-int int->ERET
)

(provide (struct-out ERET))

(define (int->ERETAA/struct i)
  (list (bitwise-bit-field i 10 11))
)

(define ERETAA-head #x6b)

(define (int->ERETAA i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) ERETAA-head)
    (equal? (bitwise-bit-field i 21 25) #x4)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 12 16) 0)
    (equal? (bitwise-bit-field i 11 12) 1)
    (equal? (bitwise-bit-field i 5 10) #x1f)
    (equal? (bitwise-bit-field i 0 5) #x1f)
    ) #f]
    [else (apply ERETAA (int->ERETAA/struct i))])
)

(define (ERETAA->int e)
  (match-define (ERETAA m) e)
  (bitwise-ior
    (arithmetic-shift ERETAA-head 25)
    (arithmetic-shift #x4 21)
    (arithmetic-shift #x1f 16)
    (arithmetic-shift 1 11)
    (arithmetic-shift m 10)
    (arithmetic-shift #x1f 5)
    #x1f
  )
)

(struct ERETAA (m)
  #:transparent
  #:property prop:in-feature #hash((FEAT_PAuth . #t))
  #:property prop:into-int ERETAA->int
  #:property prop:try-from-int int->ERETAA
)

(provide (struct-out ERETAA))