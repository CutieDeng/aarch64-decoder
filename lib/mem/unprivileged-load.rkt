#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LDTR/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDTR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply LDTR (int->LDTR/struct i))])
)

(define (LDTR->int l)
  (match-define (LDTR size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDTR (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDTR->int
  #:property prop:try-from-int int->LDTR
)

(provide (struct-out LDTR))

(define int->LDTRB/struct int->LDTR/struct)

(define (int->LDTRB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply LDTRB (int->LDTRB/struct i))])
)

(define (LDTRB->int l)
  (match-define (LDTRB size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDTRB (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDTRB->int
  #:property prop:try-from-int int->LDTRB
)

(provide (struct-out LDTRB))

(define int->LDTRSB/struct int->LDTR/struct)

(define (int->LDTRSB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply LDTRSB (int->LDTRSB/struct i))])
)

(define (LDTRSB->int l)
  (match-define (LDTRSB size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDTRSB (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDTRSB->int
  #:property prop:try-from-int int->LDTRSB
)

(provide (struct-out LDTRSB))

(define int->LDTRH/struct int->LDTR/struct)

(define (int->LDTRH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) #x2)
  ) #f]
  [else (apply LDTRH (int->LDTRH/struct i))])
)

(define (LDTRH->int l)
  (match-define (LDTRH size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDTRH (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDTRH->int
  #:property prop:try-from-int int->LDTRH
)

(provide (struct-out LDTRH))
