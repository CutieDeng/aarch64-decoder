#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->STR/NEON/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STR/NEON i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply STR/NEON (int->STR/NEON/struct i))])
)

(define (STR/NEON->int l)
  (match-define (STR/NEON size opc rm option s rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift #x1 26)
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

(struct STR/NEON (size opc rm option s rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STR/NEON->int
  #:property prop:try-from-int int->STR/NEON
)

(provide (struct-out STR/NEON))

(define (int->STR/NEON/Post/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STR/NEON/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply STR/NEON/Post (int->STR/NEON/Post/struct i))])
)

(define (STR/NEON/Post->int l)
  (match-define (STR/NEON/Post size opc imm9 rn rt) l)
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

(struct STR/NEON/Post (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STR/NEON/Post->int
  #:property prop:try-from-int int->STR/NEON/Post
)

(provide (struct-out STR/NEON/Post))

(define int->STR/NEON/Pre/struct int->STR/NEON/Post/struct)

(define (int->STR/NEON/Pre i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) #x3)
  ) #f]
  [else (apply STR/NEON/Pre (int->STR/NEON/Pre/struct i))])
)

(define (STR/NEON/Pre->int l)
  (match-define (STR/NEON/Pre size opc imm9 rn rt) l)
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

(struct STR/NEON/Pre (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STR/NEON/Pre->int
  #:property prop:try-from-int int->STR/NEON/Pre
)

(provide (struct-out STR/NEON/Pre))

(define (int->STR/NEON/Unsigned/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 10 22)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)


(define (int->STR/NEON/Unsigned i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 24 26) 1)
    (equal? (bitwise-bit-field i 22 23) 0)
  ) #f]
  [else (apply STR/NEON/Unsigned (int->STR/NEON/Unsigned/struct i))])
)

(define (STR/NEON/Unsigned->int l)
  (match-define (STR/NEON/Unsigned size opc imm12 rn rt) l)
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

(struct STR/NEON/Unsigned (size opc imm12 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STR/NEON/Unsigned->int
  #:property prop:try-from-int int->STR/NEON/Unsigned
)

(provide (struct-out STR/NEON/Unsigned))

(define (int->STUR/NEON/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STUR/NEON i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply STUR/NEON (int->STUR/NEON/struct i))])
)

(define (STUR/NEON->int l)
  (match-define (STUR/NEON size opc imm9 rn rt) l)
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

(struct STUR/NEON (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STUR/NEON->int
  #:property prop:try-from-int int->STUR/NEON
)

(provide (struct-out STUR/NEON))

(define (int->STP/NEON/Post/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 15 22)
    (bitwise-bit-field i 10 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STP/NEON/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x5)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 23 26) 1)
    (equal? (bitwise-bit-field i 22 23) 0)
  ) #f]
  [else (apply STP/NEON/Post (int->STP/NEON/Post/struct i))])
)

(define (STP/NEON/Post->int l)
  (match-define (STP/NEON/Post opc l imm7 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift opc 30)
    (arithmetic-shift #x5 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift #x1 23)
    (arithmetic-shift l 22)
    (arithmetic-shift imm7 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STP/NEON/Post (opc l imm7 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STP/NEON/Post->int
  #:property prop:try-from-int int->STP/NEON/Post
)

(provide (struct-out STP/NEON/Post))

(define int->STP/NEON/Pre/struct int->STP/NEON/Post/struct)

(define (int->STP/NEON/Pre i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x5)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 23 26) #x3)
    (equal? (bitwise-bit-field i 22 23) 0)
  ) #f]
  [else (apply STP/NEON/Pre (int->STP/NEON/Pre/struct i))])
)

(define (STP/NEON/Pre->int l)
  (match-define (STP/NEON/Pre opc l imm7 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift opc 30)
    (arithmetic-shift #x5 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift #x3 23)
    (arithmetic-shift l 22)
    (arithmetic-shift imm7 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STP/NEON/Pre (opc l imm7 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STP/NEON/Pre->int
  #:property prop:try-from-int int->STP/NEON/Pre
)

(provide (struct-out STP/NEON/Pre))

(define int->STP/NEON/Signed/struct int->STP/NEON/Post/struct)

(define (int->STP/NEON/Signed i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x5)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 23 26) #x2)
    (equal? (bitwise-bit-field i 22 23) 0)
  ) #f]
  [else (apply STP/NEON/Signed (int->STP/NEON/Signed/struct i))])
)

(define (STP/NEON/Signed->int l)
  (match-define (STP/NEON/Signed opc l imm7 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift opc 30)
    (arithmetic-shift #x5 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift #x2 23)
    (arithmetic-shift l 22)
    (arithmetic-shift imm7 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STP/NEON/Signed (opc l imm7 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STP/NEON/Signed->int
  #:property prop:try-from-int int->STP/NEON/Signed
)

(provide (struct-out STP/NEON/Signed))

(define int->STNP/NEON/struct int->STP/NEON/Post/struct)

(define (int->STNP/NEON i)
  (cond [(nand
    (equal? (bitwise-bit-field i 27 30) #x5)
    (equal? (bitwise-bit-field i 26 27) 1)
    (equal? (bitwise-bit-field i 23 26) 0)
    (equal? (bitwise-bit-field i 22 23) 0)
  ) #f]
  [else (apply STNP/NEON (int->STNP/NEON/struct i))])
)

(define (STNP/NEON->int l)
  (match-define (STNP/NEON opc l imm7 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift opc 30)
    (arithmetic-shift #x5 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift l 22)
    (arithmetic-shift imm7 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STNP/NEON (opc l imm7 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STNP/NEON->int
  #:property prop:try-from-int int->STNP/NEON
)

(provide (struct-out STNP/NEON))
