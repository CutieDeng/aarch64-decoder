#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LDADD/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 23 24)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 12 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDADD i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDADD (int->LDADD/struct i))])
)

(define (LDADD->int ldadd)
  (match-define (LDADD size a r rs opc rn rt) ldadd)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift 1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift opc 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDADD (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDADD->int
  #:property prop:try-from-int int->LDADD
)

(provide (struct-out LDADD))

(define int->LDADDB/struct int->LDADD/struct)

(define (int->LDADDB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDADDB (int->LDADDB/struct i))])
)

(define (LDADDB->int ldadd)
  (match-define (LDADDB size a r rs opc rn rt) ldadd)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift 1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift opc 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDADDB (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDADDB->int
  #:property prop:try-from-int int->LDADDB
)

(provide (struct-out LDADDB))
