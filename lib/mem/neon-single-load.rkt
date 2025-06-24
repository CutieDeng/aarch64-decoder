#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LD1R/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 21 22)
    (bitwise-bit-field i 16 17)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 10 12)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LD1R i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1a)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 17 21) 0)
    (equal? (bitwise-bit-field i 16 17) 0)
    (equal? (bitwise-bit-field i 13 16) #x6)
    (equal? (bitwise-bit-field i 12 13) 0)
  ) #f]
  [else (apply LD1R (int->LD1R/struct i))])
)

(define (LD1R->int ld1)
  (match-define (LD1R q l r o2 opcode s size rn rt) ld1)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x1a 23)
    (arithmetic-shift l 22)
    (arithmetic-shift r 21)
    (arithmetic-shift o2 16)
    (arithmetic-shift opcode 13)
    (arithmetic-shift s 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LD1R (q l r o2 opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD1R->int
  #:property prop:try-from-int int->LD1R
)

(provide (struct-out LD1R))

(define (int->LD1R/Post/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 21 22)
    (bitwise-bit-field i 16 17)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 10 12)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LD1R/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1a)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 17 21) 0)
    (equal? (bitwise-bit-field i 16 17) 0)
    (equal? (bitwise-bit-field i 13 16) #x6)
    (equal? (bitwise-bit-field i 12 13) 0)
  ) #f]
  [else (apply LD1R/Post (int->LD1R/Post/struct i))])
)

(define (LD1R/Post->int ld1)
  (match-define (LD1R/Post q l r rm opcode s size rn rt) ld1)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x1b 23)
    (arithmetic-shift l 22)
    (arithmetic-shift r 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift opcode 13)
    (arithmetic-shift s 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LD1R/Post (q l r rm opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD1R/Post->int
  #:property prop:try-from-int int->LD1R/Post
)

(provide (struct-out LD1R/Post))
