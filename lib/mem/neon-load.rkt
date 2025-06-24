#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LDR/NEON/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDR/NEON i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply LDR/NEON (int->LDR/NEON/struct i))])
)

(define (LDR/NEON->int l)
  (match-define (LDR/NEON size opc rm option s rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift opc 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift option 13)
    (arithmetic-shift s 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDR/NEON (size opc rm option s rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDR/NEON->int
  #:property prop:try-from-int int->LDR/NEON
)

(provide (struct-out LDR/NEON))

(define (int->LDR/NEON/Post/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDR/NEON/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply LDR/NEON/Post (int->LDR/NEON/Post/struct i))])
)

(define (LDR/NEON/Post->int l)
  (match-define (LDR/NEON/Post size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDR/NEON/Post (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDR/NEON/Post->int
  #:property prop:try-from-int int->LDR/NEON/Post
)

(provide (struct-out LDR/NEON/Post))

(define int->LDR/NEON/Pre/struct int->LDR/NEON/Post/struct)

(define (int->LDR/NEON/Pre i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) #x3)
  ) #f]
  [else (apply LDR/NEON/Pre (int->LDR/NEON/Pre/struct i))])
)

(define (LDR/NEON/Pre->int l)
  (match-define (LDR/NEON/Pre size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x3 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDR/NEON/Pre (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDR/NEON/Pre->int
  #:property prop:try-from-int int->LDR/NEON/Pre
)

(provide (struct-out LDR/NEON/Pre))

(define (int->LDR/NEON/Unsigned/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 10 22)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)


(define (int->LDR/NEON/Unsigned i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 24 26) 1)
    (equal? (bitwise-bit-field i 22 23) 1)
  ) #f]
  [else (apply LDR/NEON/Unsigned (int->LDR/NEON/Unsigned/struct i))])
)

(define (LDR/NEON/Unsigned->int l)
  (match-define (LDR/NEON/Unsigned size opc imm12 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift #x1 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm12 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDR/NEON/Unsigned (size opc imm12 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDR/NEON/Unsigned->int
  #:property prop:try-from-int int->LDR/NEON/Unsigned
)

(provide (struct-out LDR/NEON/Unsigned))

(define (int->LDR/NEON/l/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 5 24)
    (bitwise-bit-field i 0 5))
)

(define (int->LDR/NEON/l i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 24 26) 0)
  ) #f]
  [else (apply LDR/NEON/l (int->LDR/NEON/l/struct i))])
)

(define (LDR/NEON/l->int l)
  (match-define (LDR/NEON/l opc imm19 rt) l)
  (bitwise-ior
    (arithmetic-shift opc 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift imm19 5)
    rt
  )
)

(struct LDR/NEON/l (opc imm19 rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDR/NEON/l->int
  #:property prop:try-from-int int->LDR/NEON/l
)

(provide (struct-out LDR/NEON/l))

(define (int->LDUR/NEON/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDUR/NEON i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDUR/NEON (int->LDUR/NEON/struct i))])
)

(define (LDUR/NEON->int l)
  (match-define (LDUR/NEON size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDUR/NEON (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDUR/NEON->int
  #:property prop:try-from-int int->LDUR/NEON
)

(provide (struct-out LDUR/NEON))
