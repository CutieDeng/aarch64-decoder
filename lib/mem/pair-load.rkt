#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LDP/Post/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 15 22)
    (bitwise-bit-field i 10 15)
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

(define int->LDP/Pre/struct int->LDP/Post/struct)

(define (int->LDP/Pre i)
  (cond [(nand (equal? (bitwise-bit-field i 30 31) 0)
    (equal? (bitwise-bit-field i 27 30) #x5)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 23 26) #x3)
    (equal? (bitwise-bit-field i 22 23) 1)
  ) #f]
  [else (apply LDP/Pre (int->LDP/Pre/struct i))])
)

(define (LDP/Pre->int l)
  (match-define (LDP/Pre opc l imm7 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift opc 30)
    (arithmetic-shift #x5 27)
    (arithmetic-shift #x3 23)
    (arithmetic-shift l 22)
    (arithmetic-shift imm7 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDP/Pre (opc l imm7 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDP/Pre->int
  #:property prop:try-from-int int->LDP/Pre
)

(provide (struct-out LDP/Pre))

(define int->LDP/Signed/struct int->LDP/Post/struct)

(define (int->LDP/Signed i)
  (cond [(nand (equal? (bitwise-bit-field i 30 31) 0)
    (equal? (bitwise-bit-field i 27 30) #x5)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 23 26) #x2)
    (equal? (bitwise-bit-field i 22 23) 1)
  ) #f]
  [else (apply LDP/Signed (int->LDP/Signed/struct i))])
)

(define (LDP/Signed->int l)
  (match-define (LDP/Signed opc l imm7 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift opc 30)
    (arithmetic-shift #x5 27)
    (arithmetic-shift #x2 23)
    (arithmetic-shift l 22)
    (arithmetic-shift imm7 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDP/Signed (opc l imm7 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDP/Signed->int
  #:property prop:try-from-int int->LDP/Signed
)

(provide (struct-out LDP/Signed))
