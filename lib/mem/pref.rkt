#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->PRFM/r/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->PRFM/r i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) #x3)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) #x2)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 14 15) #x1)
    (equal? (bitwise-bit-field i 10 12) #x2)
    (not (equal? (bitwise-bit-field i 3 5) #x3))
  ) #f]
  [else (apply PRFM/r (int->PRFM/r/struct i))])
)

(define (PRFM/r->int prfm)
  (match-define (PRFM/r size opc rm option s rn rt) prfm)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift option 13)
    (arithmetic-shift s 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct PRFM/r (size opc rm option s rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int PRFM/r->int
  #:property prop:try-from-int int->PRFM/r
)

(provide (struct-out PRFM/r))

(define (int->PRFM/i/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 10 22)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->PRFM/i i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) #x3)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 22 24) #x2)
  ) #f]
  [else (apply PRFM/i (int->PRFM/i/struct i))])
)

(define (PRFM/i->int prfm)
  (match-define (PRFM/i size opc imm12 rn rt) prfm)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm12 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct PRFM/i (size opc imm12 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int PRFM/i->int
  #:property prop:try-from-int int->PRFM/i
)

(provide (struct-out PRFM/i))
