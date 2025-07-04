#lang racket

(require "../../util/in-feature.rkt")
(require "../../util/into-int.rkt")
(require "../../util/try-from-int.rkt")

(define (int->LD1B/struct i)
  (list
    (bitwise-bit-field i 21 24)
    (bitwise-bit-field i 16 20)
    (bitwise-bit-field i 10 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LD1B i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 23 25) #x0)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x5)
  ) #f]
  [else (apply LD1B (int->LD1B/struct i))])
)

(define (LD1B->int ld1)
  (match-define (LD1B dtype imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x5 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1B (dtype imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature #f
  #:property prop:into-int LD1B->int
  #:property prop:try-from-int int->LD1B
)

(provide (struct-out LD1B))

(define (int->LD1D/struct i)
  (list
    (bitwise-bit-field i 21 24)
    (bitwise-bit-field i 16 20)
    (bitwise-bit-field i 10 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LD1D i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 21 25) #xf)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x5)
  ) #f]
  [else (apply LD1D (int->LD1D/struct i))])
)

(define (LD1D->int ld1)
  (match-define (LD1D dtype imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x5 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1D (dtype imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature #f
  #:property prop:into-int LD1D->int
  #:property prop:try-from-int int->LD1D
)

(provide (struct-out LD1D))

(define (int->LD1H/struct i)
  (list
    (bitwise-bit-field i 21 24)
    (bitwise-bit-field i 16 20)
    (bitwise-bit-field i 10 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LD1H i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 23 25) #x1)
    (not (equal? (bitwise-bit-field i 21 23) #x0))
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x5)
  ) #f]
  [else (apply LD1H (int->LD1H/struct i))])
)

(define (LD1H->int ld1)
  (match-define (LD1H dtype imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x5 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1H (dtype imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature #f
  #:property prop:into-int LD1H->int
  #:property prop:try-from-int int->LD1H
)

(provide (struct-out LD1H))
