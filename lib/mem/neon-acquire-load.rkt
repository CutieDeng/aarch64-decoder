#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LDAP1/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 21 22)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 10 12)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDAP1 i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1a)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 17 21) #x0)
    (equal? (bitwise-bit-field i 16 17) 1)
    (equal? (bitwise-bit-field i 13 16) #x4)
    (equal? (bitwise-bit-field i 12 13) 0)
    (equal? (bitwise-bit-field i 10 12) 1)
  ) #f]
  [else (apply LDAP1 (int->LDAP1/struct i))])
)

(define (LDAP1->int ldap1)
  (match-define (LDAP1 q l r opcode s size rn rt) ldap1)
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

(struct LDAP1 (q l r opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC3 . #t))
  #:property prop:into-int LDAP1->int
  #:property prop:try-from-int int->LDAP1
)

(provide (struct-out LDAP1))
