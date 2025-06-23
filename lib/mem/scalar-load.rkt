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

(define (LDRB/r->int l)
  (match-define (LDRB/r size vr opc rm option s rn rt) l)
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

(struct LDRB/r (size vr opc rm option s rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRB/r->int
  #:property prop:try-from-int int->LDRB/r
)

(provide (struct-out LDRB/r))

(define int->LDRB/i/Post/struct int->LDR/i/Post/struct)

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

(define (LDRB/i/Post->int l)
  (match-define (LDRB/i/Post size vr opc imm9 rn rt) l)
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

(struct LDRB/i/Post (size vr opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRB/i/Post->int
  #:property prop:try-from-int int->LDRB/i/Post
)

(provide (struct-out LDRB/i/Post))

(define int->LDRB/i/Pre/struct int->LDR/i/Post/struct)

(define (int->LDRB/i/Pre i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) #x3)
  ) #f]
  [else (apply LDRB/i/Pre (int->LDRB/i/Pre/struct i))])
)

(define (LDRB/i/Pre->int l)
  (match-define (LDRB/i/Pre size vr opc imm9 rn rt) l)
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

(struct LDRB/i/Pre (size vr opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRB/i/Pre->int
  #:property prop:try-from-int int->LDRB/i/Pre
)

(provide (struct-out LDRB/i/Pre))

(define int->LDRB/i/Unsigned/struct int->LDR/i/Unsigned/struct)

(define (int->LDRB/i/Unsigned i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 1)
    (equal? (bitwise-bit-field i 22 24) 1)
  ) #f]
  [else (apply LDRB/i/Unsigned (int->LDRB/i/Unsigned/struct i))])
)

(define (LDRB/i/Unsigned->int l)
  (match-define (LDRB/i/Unsigned size vr opc imm12 rn rt) l)
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

(struct LDRB/i/Unsigned (size vr opc imm12 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRB/i/Unsigned->int
  #:property prop:try-from-int int->LDRB/i/Unsigned
)

(provide (struct-out LDRB/i/Unsigned))

(define int->LDRSB/r/struct int->LDR/r/struct)

(define (int->LDRSB/r i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply LDRSB/r (int->LDRSB/r/struct i))])
)

(define (LDRSB/r->int l)
  (match-define (LDRSB/r size vr opc rm option s rn rt) l)
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

(struct LDRSB/r (size vr opc rm option s rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRSB/r->int
  #:property prop:try-from-int int->LDRSB/r
)

(provide (struct-out LDRSB/r))

(define (int->LDRH/r/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDRH/r i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply LDRH/r (int->LDRH/r/struct i))])
)

(define (LDRH/r->int l)
  (match-define (LDRH/r size opc rm option s rn rt) l)
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

(struct LDRH/r (size opc rm option s rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRH/r->int
  #:property prop:try-from-int int->LDRH/r
)

(provide (struct-out LDRH/r))

(define (int->LDRSB/i/Post/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDRSB/i/Post i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 1)
  ) #f]
  [else (apply LDRSB/i/Post (int->LDRSB/i/Post/struct i))])
)

(define (LDRSB/i/Post->int l)
  (match-define (LDRSB/i/Post size opc imm9 rn rt) l)
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

(struct LDRSB/i/Post (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRSB/i/Post->int
  #:property prop:try-from-int int->LDRSB/i/Post
)

(provide (struct-out LDRSB/i/Post))

(define int->LDRSB/i/Pre/struct int->LDRSB/i/Post/struct)

(define (int->LDRSB/i/Pre i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) #x3)
  ) #f]
  [else (apply LDRSB/i/Pre (int->LDRSB/i/Pre/struct i))])
)

(define (LDRSB/i/Pre->int l)
  (match-define (LDRSB/i/Pre size opc imm9 rn rt) l)
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

(struct LDRSB/i/Pre (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRSB/i/Pre->int
  #:property prop:try-from-int int->LDRSB/i/Pre
)

(provide (struct-out LDRSB/i/Pre))

(define int->LDRSB/i/Unsigned/struct int->LDRSB/i/Post/struct)

(define (int->LDRSB/i/Unsigned i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 1)
    (equal? (bitwise-bit-field i 23 24) 1)
  ) #f]
  [else (apply LDRSB/i/Unsigned (int->LDRSB/i/Unsigned/struct i))])
)

(define (LDRSB/i/Unsigned->int l)
  (match-define (LDRSB/i/Unsigned size opc imm12 rn rt) l)
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

(struct LDRSB/i/Unsigned (size opc imm12 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRSB/i/Unsigned->int
  #:property prop:try-from-int int->LDRSB/i/Unsigned
)

(provide (struct-out LDRSB/i/Unsigned))

(define int->LDRH/i/Post/struct int->LDRSB/i/Post/struct)

(define (int->LDRH/i/Post i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 1)
  ) #f]
  [else (apply LDRH/i/Post (int->LDRH/i/Post/struct i))])
)

(define (LDRH/i/Post->int l)
  (match-define (LDRH/i/Post size opc imm9 rn rt) l)
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

(struct LDRH/i/Post (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRH/i/Post->int
  #:property prop:try-from-int int->LDRH/i/Post
)

(provide (struct-out LDRH/i/Post))

(define int->LDRH/i/Pre/struct int->LDRSB/i/Post/struct)

(define (int->LDRH/i/Pre i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) #x3)
  ) #f]
  [else (apply LDRH/i/Pre (int->LDRH/i/Pre/struct i))])
)

(define (LDRH/i/Pre->int l)
  (match-define (LDRH/i/Pre size opc imm9 rn rt) l)
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

(struct LDRH/i/Pre (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRH/i/Pre->int
  #:property prop:try-from-int int->LDRH/i/Pre
)

(provide (struct-out LDRH/i/Pre))

(define int->LDRH/i/Unsigned/struct int->LDRSB/i/Post/struct)

(define (int->LDRH/i/Unsigned i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 1)
    (equal? (bitwise-bit-field i 22 24) 1)
  ) #f]
  [else (apply LDRH/i/Unsigned (int->LDRH/i/Unsigned/struct i))])
)

(define (LDRH/i/Unsigned->int l)
  (match-define (LDRH/i/Unsigned size opc imm12 rn rt) l)
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

(struct LDRH/i/Unsigned (size opc imm12 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRH/i/Unsigned->int
  #:property prop:try-from-int int->LDRH/i/Unsigned
)

(provide (struct-out LDRH/i/Unsigned))

(define int->LDRSH/r/struct int->LDRH/r/struct)

(define (int->LDRSH/r i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 12) 2)
  ) #f]
  [else (apply LDRSH/r (int->LDRSH/r/struct i))])
)

(define (LDRSH/r->int l)
  (match-define (LDRSH/r size opc rm option s rn rt) l)
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

(struct LDRSH/r (size opc rm option s rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDRSH/r->int
  #:property prop:try-from-int int->LDRSH/r
)

(provide (struct-out LDRSH/r))

