#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->STL1/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 21 22)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 10 12)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STL1 i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1a)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 17 21) #x0)
    (equal? (bitwise-bit-field i 16 17) 1)
    (equal? (bitwise-bit-field i 13 16) #x4)
    (equal? (bitwise-bit-field i 12 13) 0)
    (equal? (bitwise-bit-field i 10 12) 1)
  ) #f]
  [else (apply STL1 (int->STL1/struct i))])
)

(define (STL1->int stl1)
  (match-define (STL1 q l r opcode s size rn rt) stl1)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x1a 23)
    (arithmetic-shift l 22)
    (arithmetic-shift r 21)
    (arithmetic-shift #x1 16)
    (arithmetic-shift opcode 13)
    (arithmetic-shift s 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STL1 (q l r opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC3 . #t))
  #:property prop:into-int STL1->int
  #:property prop:try-from-int int->STL1
)

(provide (struct-out STL1))

(define (int->STLUR/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STLUR i)
  (cond [(nand
    (equal? (bitwise-bit-field i 24 30) #x1d)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply STLUR (int->STLUR/struct i))])
)

(define (STLUR->int stlur)
  (match-define (STLUR size opc imm9 rn rt) stlur)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x1d 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STLUR (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC3 . #t))
  #:property prop:into-int STLUR->int
  #:property prop:try-from-int int->STLUR
)

(provide (struct-out STLUR))
