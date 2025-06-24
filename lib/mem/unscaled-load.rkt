#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LDUR/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDUR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDUR (int->LDUR/struct i))])
)

(define (LDUR->int l)
  (match-define (LDUR size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDUR (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDUR->int
  #:property prop:try-from-int int->LDUR
)

(provide (struct-out LDUR))

(define int->LDURB/struct int->LDUR/struct)

(define (int->LDURB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDURB (int->LDURB/struct i))])
)

(define (LDURB->int l)
  (match-define (LDURB size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDURB (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDURB->int
  #:property prop:try-from-int int->LDURB
)

(provide (struct-out LDURB))

(define int->LDURSB/struct int->LDUR/struct)

(define (int->LDURSB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDURSB (int->LDURSB/struct i))])
)

(define (LDURSB->int l)
  (match-define (LDURSB size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDURSB (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDURSB->int
  #:property prop:try-from-int int->LDURSB
)

(provide (struct-out LDURSB))

(define int->LDURH/struct int->LDUR/struct)

(define (int->LDURH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDURH (int->LDURH/struct i))])
)

(define (LDURH->int l)
  (match-define (LDURH size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDURH (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDURH->int
  #:property prop:try-from-int int->LDURH
)

(provide (struct-out LDURH))

(define int->LDURSH/struct int->LDUR/struct)

(define (int->LDURSH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDURSH (int->LDURSH/struct i))])
)

(define (LDURSH->int l)
  (match-define (LDURSH size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDURSH (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDURSH->int
  #:property prop:try-from-int int->LDURSH
)

(provide (struct-out LDURSH))

(define int->LDURSW/struct int->LDUR/struct)

(define (int->LDURSW i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) #x2)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 22 24) #x2)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDURSW (int->LDURSW/struct i))])
)

(define (LDURSW->int l)
  (match-define (LDURSW size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDURSW (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDURSW->int
  #:property prop:try-from-int int->LDURSW
)

(provide (struct-out LDURSW))
