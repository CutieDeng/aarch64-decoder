#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->STR/r/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STR/r i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply STR/r (int->STR/r/struct i))])
)

(define (STR/r->int l)
  (match-define (STR/r size opc rm option s rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift option 13)
    (arithmetic-shift s 12)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STR/r (size opc rm option s rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STR/r->int
  #:property prop:try-from-int int->STR/r
)

(provide (struct-out STR/r))

(define (int->STR/i/Post/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STR/i/Post i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 1)
  ) #f]
  [else (apply STR/i/Post (int->STR/i/Post/struct i))])
)

(define (STR/i/Post->int l)
  (match-define (STR/i/Post size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STR/i/Post (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STR/i/Post->int
  #:property prop:try-from-int int->STR/i/Post
)

(provide (struct-out STR/i/Post))

(define int->STR/i/Pre/struct int->STR/i/Post/struct)

(define (int->STR/i/Pre i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) #x3)
  ) #f]
  [else (apply STR/i/Pre (int->STR/i/Pre/struct i))])
)

(define (STR/i/Pre->int l)
  (match-define (STR/i/Pre size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x3 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STR/i/Pre (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STR/i/Pre->int
  #:property prop:try-from-int int->STR/i/Pre
)

(provide (struct-out STR/i/Pre))

(define (int->STR/i/Unsigned/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 10 22)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5)
  )
)

(define (int->STR/i/Unsigned i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 1)
    (equal? (bitwise-bit-field i 22 24) 0)
  ) #f]
  [else (apply STR/i/Unsigned (int->STR/i/Unsigned/struct i))])
)

(define (STR/i/Unsigned->int l)
  (match-define (STR/i/Unsigned size opc imm12 rn rt) l)
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

(struct STR/i/Unsigned (size opc imm12 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STR/i/Unsigned->int
  #:property prop:try-from-int int->STR/i/Unsigned
)

(provide (struct-out STR/i/Unsigned))

(define int->STRB/r/struct int->STR/r/struct)

(define (int->STRB/r i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply STRB/r (int->STRB/r/struct i))])
)

(define (STRB/r->int l)
  (match-define (STRB/r size opc rm option s rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift option 13)
    (arithmetic-shift s 12)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STRB/r (size opc rm option s rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STRB/r->int
  #:property prop:try-from-int int->STRB/r
)

(provide (struct-out STRB/r))

(define int->STRB/i/Post/struct int->STR/i/Post/struct)

(define (int->STRB/i/Post i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 1)
  ) #f]
  [else (apply STRB/i/Post (int->STRB/i/Post/struct i))])
)

(define (STRB/i/Post->int l)
  (match-define (STRB/i/Post size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STRB/i/Post (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STRB/i/Post->int
  #:property prop:try-from-int int->STRB/i/Post
)

(provide (struct-out STRB/i/Post))

(define int->STRB/i/Pre/struct int->STR/i/Post/struct)

(define (int->STRB/i/Pre i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) #x3)
  ) #f]
  [else (apply STRB/i/Pre (int->STRB/i/Pre/struct i))])
)

(define (STRB/i/Pre->int l)
  (match-define (STRB/i/Pre size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x3 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STRB/i/Pre (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STRB/i/Pre->int
  #:property prop:try-from-int int->STRB/i/Pre
)

(provide (struct-out STRB/i/Pre))

(define int->STRB/i/Unsigned/struct int->STR/i/Unsigned/struct)

(define (int->STRB/i/Unsigned i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 1)
    (equal? (bitwise-bit-field i 22 24) 0)
  ) #f]
  [else (apply STRB/i/Unsigned (int->STRB/i/Unsigned/struct i))])
)

(define (STRB/i/Unsigned->int l)
  (match-define (STRB/i/Unsigned size opc imm12 rn rt) l)
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

(struct STRB/i/Unsigned (size opc imm12 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STRB/i/Unsigned->int
  #:property prop:try-from-int int->STRB/i/Unsigned
)

(provide (struct-out STRB/i/Unsigned))
