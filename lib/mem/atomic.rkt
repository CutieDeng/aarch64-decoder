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

(define int->LDADDH/struct int->LDADD/struct)

(define (int->LDADDH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDADDH (int->LDADDH/struct i))])
)

(define (LDADDH->int ldadd)
  (match-define (LDADDH size a r rs opc rn rt) ldadd)
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

(struct LDADDH (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDADDH->int
  #:property prop:try-from-int int->LDADDH
)

(provide (struct-out LDADDH))

(define int->LDCLR/struct int->LDADD/struct)

(define (int->LDCLR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) 1)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDCLR (int->LDCLR/struct i))])
)

(define (LDCLR->int ldadd)
  (match-define (LDCLR size a r rs opc rn rt) ldadd)
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

(struct LDCLR (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDCLR->int
  #:property prop:try-from-int int->LDCLR
)

(provide (struct-out LDCLR))

(define int->LDCLRB/struct int->LDCLR/struct)

(define (int->LDCLRB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) 1)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDCLRB (int->LDCLRB/struct i))])
)

(define (LDCLRB->int ldadd)
  (match-define (LDCLRB size a r rs opc rn rt) ldadd)
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

(struct LDCLRB (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDCLRB->int
  #:property prop:try-from-int int->LDCLRB
)

(provide (struct-out LDCLRB))

(define int->LDCLRH/struct int->LDCLR/struct)

(define (int->LDCLRH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) 1)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDCLRH (int->LDCLRH/struct i))])
)

(define (LDCLRH->int ldadd)
  (match-define (LDCLRH size a r rs opc rn rt) ldadd)
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

(struct LDCLRH (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDCLRH->int
  #:property prop:try-from-int int->LDCLRH
)

(provide (struct-out LDCLRH))

(define (int->LDCLRP/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 23 24)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 12 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDCLRP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 30 31) 0)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) 1)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDCLRP (int->LDCLRP/struct i))])
)

(define (LDCLRP->int ldclr)
  (match-define (LDCLRP s a r rt2 o3 opc rn rt) ldclr)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift 1 21)
    (arithmetic-shift rt2 16)
    (arithmetic-shift o3 15)
    (arithmetic-shift opc 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDCLRP (s a r rt2 o3 opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDCLRP->int
  #:property prop:try-from-int int->LDCLRP
)

(provide (struct-out LDCLRP))
