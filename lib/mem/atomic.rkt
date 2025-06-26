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
  #:property prop:in-feature #hash((FEAT_LSE128 . #t))
  #:property prop:into-int LDCLRP->int
  #:property prop:try-from-int int->LDCLRP
)

(provide (struct-out LDCLRP))

(define int->LDEOR/struct int->LDADD/struct)

(define (int->LDEOR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x2)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDEOR (int->LDEOR/struct i))])
)

(define (LDEOR->int ldadd)
  (match-define (LDEOR size a r rs opc rn rt) ldadd)
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

(struct LDEOR (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDEOR->int
  #:property prop:try-from-int int->LDEOR
)

(provide (struct-out LDEOR))

(define int->LDEORB/struct int->LDEOR/struct)

(define (int->LDEORB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x2)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDEORB (int->LDEORB/struct i))])
)

(define (LDEORB->int ldadd)
  (match-define (LDEORB size a r rs opc rn rt) ldadd)
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

(struct LDEORB (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDEORB->int
  #:property prop:try-from-int int->LDEORB
)

(provide (struct-out LDEORB))

(define int->LDEORH/struct int->LDEOR/struct)

(define (int->LDEORH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x2)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDEORH (int->LDEORH/struct i))])
)

(define (LDEORH->int ldadd)
  (match-define (LDEORH size a r rs opc rn rt) ldadd)
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

(struct LDEORH (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDEORH->int
  #:property prop:try-from-int int->LDEORH
)

(provide (struct-out LDEORH))

(define int->LDSET/struct int->LDADD/struct)

(define (int->LDSET i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x3)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDSET (int->LDSET/struct i))])
)

(define (LDSET->int ldadd)
  (match-define (LDSET size a r rs opc rn rt) ldadd)
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

(struct LDSET (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDSET->int
  #:property prop:try-from-int int->LDSET
)

(provide (struct-out LDSET))

(define int->LDSETB/struct int->LDSET/struct)

(define (int->LDSETB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x3)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDSETB (int->LDSETB/struct i))])
)

(define (LDSETB->int ldadd)
  (match-define (LDSETB size a r rs opc rn rt) ldadd)
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

(struct LDSETB (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDSETB->int
  #:property prop:try-from-int int->LDSETB
)

(provide (struct-out LDSETB))

(define int->LDSETH/struct int->LDSET/struct)

(define (int->LDSETH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x3)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDSETH (int->LDSETH/struct i))])
)

(define (LDSETH->int ldadd)
  (match-define (LDSETH size a r rs opc rn rt) ldadd)
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

(struct LDSETH (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDSETH->int
  #:property prop:try-from-int int->LDSETH
)

(provide (struct-out LDSETH))

(define (int->LDSETP/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 23 24)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 12 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDSETP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 30 31) 0)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x3)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDSETP (int->LDSETP/struct i))])
)

(define (LDSETP->int ldclr)
  (match-define (LDSETP s a r rt2 o3 opc rn rt) ldclr)
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

(struct LDSETP (s a r rt2 o3 opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE128 . #t))
  #:property prop:into-int LDSETP->int
  #:property prop:try-from-int int->LDSETP
)

(provide (struct-out LDSETP))

(define int->LDSMAX/struct int->LDADD/struct)

(define (int->LDSMAX i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x4)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDSMAX (int->LDSMAX/struct i))])
)

(define (LDSMAX->int ldadd)
  (match-define (LDSMAX size a r rs opc rn rt) ldadd)
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

(struct LDSMAX (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDSMAX->int
  #:property prop:try-from-int int->LDSMAX
)

(provide (struct-out LDSMAX))

(define int->LDSMAXB/struct int->LDSMAX/struct)

(define (int->LDSMAXB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x4)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDSMAXB (int->LDSMAXB/struct i))])
)

(define (LDSMAXB->int ldadd)
  (match-define (LDSMAXB size a r rs opc rn rt) ldadd)
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

(struct LDSMAXB (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDSMAXB->int
  #:property prop:try-from-int int->LDSMAXB
)

(provide (struct-out LDSMAXB))

(define int->LDSMAXH/struct int->LDSMAX/struct)

(define (int->LDSMAXH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x4)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDSMAXH (int->LDSMAXH/struct i))])
)

(define (LDSMAXH->int ldadd)
  (match-define (LDSMAXH size a r rs opc rn rt) ldadd)
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

(struct LDSMAXH (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDSMAXH->int
  #:property prop:try-from-int int->LDSMAXH
)

(provide (struct-out LDSMAXH))

(define int->LDSMIN/struct int->LDADD/struct)

(define (int->LDSMIN i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x5)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDSMIN (int->LDSMIN/struct i))])
)

(define (LDSMIN->int ldadd)
  (match-define (LDSMIN size a r rs opc rn rt) ldadd)
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

(struct LDSMIN (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDSMIN->int
  #:property prop:try-from-int int->LDSMIN
)

(provide (struct-out LDSMIN))

(define int->LDSMINB/struct int->LDSMIN/struct)

(define (int->LDSMINB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x5)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDSMINB (int->LDSMINB/struct i))])
)

(define (LDSMINB->int ldadd)
  (match-define (LDSMINB size a r rs opc rn rt) ldadd)
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

(struct LDSMINB (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDSMINB->int
  #:property prop:try-from-int int->LDSMINB
)

(provide (struct-out LDSMINB))

(define int->LDSMINH/struct int->LDSMIN/struct)

(define (int->LDSMINH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x5)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDSMINH (int->LDSMINH/struct i))])
)

(define (LDSMINH->int ldadd)
  (match-define (LDSMINH size a r rs opc rn rt) ldadd)
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

(struct LDSMINH (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDSMINH->int
  #:property prop:try-from-int int->LDSMINH
)

(provide (struct-out LDSMINH))

(define int->LDUMAX/struct int->LDADD/struct)

(define (int->LDUMAX i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x6)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDUMAX (int->LDUMAX/struct i))])
)

(define (LDUMAX->int ldadd)
  (match-define (LDUMAX size a r rs opc rn rt) ldadd)
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

(struct LDUMAX (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDUMAX->int
  #:property prop:try-from-int int->LDUMAX
)

(provide (struct-out LDUMAX))

(define int->LDUMAXB/struct int->LDUMAX/struct)

(define (int->LDUMAXB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x6)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDUMAXB (int->LDUMAXB/struct i))])
)

(define (LDUMAXB->int ldadd)
  (match-define (LDUMAXB size a r rs opc rn rt) ldadd)
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

(struct LDUMAXB (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDUMAXB->int
  #:property prop:try-from-int int->LDUMAXB
)

(provide (struct-out LDUMAXB))

(define int->LDUMAXH/struct int->LDUMAX/struct)

(define (int->LDUMAXH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 12 15) #x6)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDUMAXH (int->LDUMAXH/struct i))])
)

(define (LDUMAXH->int ldadd)
  (match-define (LDUMAXH size a r rs opc rn rt) ldadd)
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

(struct LDUMAXH (size a r rs opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int LDUMAXH->int
  #:property prop:try-from-int int->LDUMAXH
)

(provide (struct-out LDUMAXH))
