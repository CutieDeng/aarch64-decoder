#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->STP/Post/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 15 22)
    (bitwise-bit-field i 10 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STP/Post i)
  (cond [(nand (equal? (bitwise-bit-field i 30 31) 0)
    (equal? (bitwise-bit-field i 27 30) #x5)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 23 26) 1)
    (equal? (bitwise-bit-field i 22 23) 0)
  ) #f]
  [else (apply STP/Post (int->STP/Post/struct i))])
)

(define (STP/Post->int l)
  (match-define (STP/Post opc l imm7 rt2 rn rt) l)
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

(struct STP/Post (opc l imm7 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STP/Post->int
  #:property prop:try-from-int int->STP/Post
)

(provide (struct-out STP/Post))

(define int->STP/Pre/struct int->STP/Post/struct)

(define (int->STP/Pre i)
  (cond [(nand (equal? (bitwise-bit-field i 30 31) 0)
    (equal? (bitwise-bit-field i 27 30) #x5)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 23 26) #x3)
    (equal? (bitwise-bit-field i 22 23) 0)
  ) #f]
  [else (apply STP/Pre (int->STP/Pre/struct i))])
)

(define (STP/Pre->int l)
  (match-define (STP/Pre opc l imm7 rt2 rn rt) l)
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

(struct STP/Pre (opc l imm7 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STP/Pre->int
  #:property prop:try-from-int int->STP/Pre
)

(provide (struct-out STP/Pre))

(define int->STP/Signed/struct int->STP/Post/struct)

(define (int->STP/Signed i)
  (cond [(nand (equal? (bitwise-bit-field i 30 31) 0)
    (equal? (bitwise-bit-field i 27 30) #x5)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 23 26) #x2)
    (equal? (bitwise-bit-field i 22 23) 0)
  ) #f]
  [else (apply STP/Signed (int->STP/Signed/struct i))])
)

(define (STP/Signed->int l)
  (match-define (STP/Signed opc l imm7 rt2 rn rt) l)
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

(struct STP/Signed (opc l imm7 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STP/Signed->int
  #:property prop:try-from-int int->STP/Signed
)

(provide (struct-out STP/Signed))
