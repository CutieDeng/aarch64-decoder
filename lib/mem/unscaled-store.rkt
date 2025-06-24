#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->STUR/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STUR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply STUR (int->STUR/struct i))])
)

(define (STUR->int l)
  (match-define (STUR size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STUR (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STUR->int
  #:property prop:try-from-int int->STUR
)

(provide (struct-out STUR))

(define int->STURB/struct int->STUR/struct)

(define (int->STURB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply STURB (int->STURB/struct i))])
)

(define (STURB->int l)
  (match-define (STURB size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STURB (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STURB->int
  #:property prop:try-from-int int->STURB
)

(provide (struct-out STURB))
