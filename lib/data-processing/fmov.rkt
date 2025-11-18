#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->FMOV/neon/struct i)
  (list
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 15 17)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->FMOV/neon i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 29 30) #x0)
    (equal? (bitwise-bit-field i 24 29) #x1e)
    (equal? (bitwise-bit-field i 21 22) #x1)
    (equal? (bitwise-bit-field i 17 21) #x0)
    (equal? (bitwise-bit-field i 15 17) #x0)
    (equal? (bitwise-bit-field i 10 15) #x10)
  ) #f]
  [else (apply FMOV/neon (int->FMOV/neon/struct i))])
)

(define (FMOV/neon->int f)
  (match-define (FMOV/neon ftype opc rn rd) f)
  (bitwise-ior
    (arithmetic-shift #x1e 24)
    (arithmetic-shift ftype 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift opc 15)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct FMOV/neon (ftype opc rn rd)
  #:transparent
  #:property prop:in-feature #f
  #:property prop:into-int FMOV/neon->int
  #:property prop:try-from-int int->FMOV/neon
)

(provide (struct-out FMOV/neon))

(define (int->FMOV/r/struct i)
  (list
    (bitwise-bit-field i 31 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 19 21)
    (bitwise-bit-field i 16 19)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->FMOV/r i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 29 30) #x0)
    (equal? (bitwise-bit-field i 24 29) #x1e)
    (equal? (bitwise-bit-field i 21 22) #x1)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 17 19) #x3)
    (equal? (bitwise-bit-field i 10 16) #x0)
  ) #f]
  [else (apply FMOV/r (int->FMOV/r/struct i))])
)

(define (FMOV/r->int f)
  (match-define (FMOV/r sf ftype rmode opcode rn rd) f)
  (bitwise-ior
    (arithmetic-shift sf 31)
    (arithmetic-shift #x1e 24)
    (arithmetic-shift ftype 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rmode 19)
    (arithmetic-shift opcode 16)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct FMOV/r (sf ftype rmode opcode rn rd)
  #:transparent
  #:property prop:in-feature #f
  #:property prop:into-int FMOV/r->int
  #:property prop:try-from-int int->FMOV/r
)

(provide (struct-out FMOV/r))