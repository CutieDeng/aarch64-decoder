#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LDR/r/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 26 27)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDR/r i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 12) 2)
  ) #f]
  [else (apply LDR/r (int->LDR/r/struct i))])
)

(define (LDR/r->int l)
  (match-define (LDR/r size vr opc rm option s rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift vr 26)
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

(struct LDR/r (size vr opc rm option s rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDR/r->int
  #:property prop:try-from-int int->LDR/r
)

(provide (struct-out LDR/r))

(define (option->extend option)
  (match option
    [#x010 'UXTW]
    [#x011 'LSL]
    [#x110 'SXTW]
    [#x111 'SXTX]
  )
)

(define (int->LDR/i/Post/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 26 27)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDR/i/Post i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 1)
  ) #f]
  [else (apply LDR/i/Post (int->LDR/i/Post/struct i))])
)

(define (LDR/i/Post->int l)
  (match-define (LDR/i/Post size vr opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift vr 26)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDR/i/Post (size vr opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDR/i/Post->int
  #:property prop:try-from-int int->LDR/i/Post
)

(provide (struct-out LDR/i/Post))

(define int->LDR/i/Pre/struct int->LDR/i/Post/struct)

(define (int->LDR/i/Pre i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) #x3)
  ) #f]
  [else (apply LDR/i/Pre (int->LDR/i/Pre/struct i))])
)

(define (LDR/i/Pre->int l)
  (match-define (LDR/i/Pre size vr opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift vr 26)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x3 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDR/i/Pre (size vr opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDR/i/Pre->int
  #:property prop:try-from-int int->LDR/i/Pre
)

(provide (struct-out LDR/i/Pre))

(define (int->LDR/i/Unsigned/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 26 27)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 10 22)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5)
  )
)

(define (int->LDR/i/Unsigned i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 1)
    (equal? (bitwise-bit-field i 22 24) 1)
  ) #f]
  [else (apply LDR/i/Unsigned (int->LDR/i/Unsigned/struct i))])
)

(define (LDR/i/Unsigned->int l)
  (match-define (LDR/i/Unsigned size vr opc imm12 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift vr 26)
    (arithmetic-shift #x1 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm12 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDR/i/Unsigned (size vr opc imm12 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDR/i/Unsigned->int
  #:property prop:try-from-int int->LDR/i/Unsigned
)

(provide (struct-out LDR/i/Unsigned))

(define (int->LDR/l/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 26 27)
    (bitwise-bit-field i 5 24)
    (bitwise-bit-field i 0 5)
  )
)

(define (int->LDR/l i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
  ) #f]
  [else (apply LDR/l (int->LDR/l/struct i))])
)

(define (LDR/l->int l)
  (match-define (LDR/l size vr imm19 rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift vr 26)
    (arithmetic-shift imm19 5)
    rt
  )
)

(struct LDR/l (size vr imm19 rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDR/l->int
  #:property prop:try-from-int int->LDR/l
)

(provide (struct-out LDR/l))

(define int->LDRB/r/struct int->LDR/r/struct)

(define (int->LDRB/r i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 12) 2)
  ) #f]
  [else (apply LDRB/r (int->LDRB/r/struct i))])
)

(define LDRB/r->int LDR/r->int)

(struct LDRB/r (size vr opc rm option s rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRB/r->int
  #:property prop:try-from-int int->LDRB/r
)

(provide (struct-out LDRB/i/Post))

(define int->LDRB/i/Post/struct int->LDR/r/struct)

(define (int->LDRB/i/Post i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 1)
  ) #f]
  [else (apply LDRB/i/Post (int->LDRB/i/Post/struct i))])
)

(define LDRB/i/Post->int LDR/i/Post->int)

(struct LDRB/i/Post (size vr opc rm option s rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRB/i/Post->int
  #:property prop:try-from-int int->LDRB/i/Post
)

(provide (struct-out LDRB/i/Post))