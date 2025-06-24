#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LDP/Post/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 26 27)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDP/Post i)
  (cond [(nand (equal? (bitwise-bit-field i 30 31) 0)
    (equal? (bitwise-bit-field i 27 30) #x5)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 23 26) 1)
    (equal? (bitwise-bit-field i 22 23) 1)
  ) #f]
  [else (apply LDP/Post (int->LDP/Post/struct i))])
)

(define (LDP/Post->int l)
  (match-define (LDP/Post opc l imm7 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift opc 30)
    (arithmetic-shift #x5 27)
    (arithmetic-shift #x1 23)
    (arithmetic-shift l 22)
    (arithmetic-shift imm7 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDP/Post (opc l imm7 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDP/Post->int
  #:property prop:try-from-int int->LDP/Post
)

(provide (struct-out LDP/Post))
