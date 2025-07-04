#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->CPYFP/struct i)
  (list
    (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 12 16)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->CPYFP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #x0)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFP (int->CPYFP/struct i))])
)

(define (CPYFP->int rcw)
  (match-define (CPYFP sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFP (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFP->int
  #:property prop:try-from-int int->CPYFP
)

(provide (struct-out CPYFP))

(define int->CPYFPN/struct int->CPYFP/struct)

(define (int->CPYFPN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #xc)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPN (int->CPYFPN/struct i))])
)

(define (CPYFPN->int rcw)
  (match-define (CPYFPN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPN->int
  #:property prop:try-from-int int->CPYFPN
)

(provide (struct-out CPYFPN))

(define int->CPYFPRN/struct int->CPYFP/struct)

(define (int->CPYFPRN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #x8)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPRN (int->CPYFPRN/struct i))])
)

(define (CPYFPRN->int rcw)
  (match-define (CPYFPRN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPRN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPRN->int
  #:property prop:try-from-int int->CPYFPRN
)

(provide (struct-out CPYFPRN))

(define int->CPYFPRT/struct int->CPYFP/struct)

(define (int->CPYFPRT i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #x2)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPRT (int->CPYFPRT/struct i))])
)

(define (CPYFPRT->int rcw)
  (match-define (CPYFPRT sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPRT (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPRT->int
  #:property prop:try-from-int int->CPYFPRT
)

(provide (struct-out CPYFPRT))

(define int->CPYFPRTN/struct int->CPYFP/struct)

(define (int->CPYFPRTN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #xe)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPRTN (int->CPYFPRTN/struct i))])
)

(define (CPYFPRTN->int rcw)
  (match-define (CPYFPRTN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPRTN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPRTN->int
  #:property prop:try-from-int int->CPYFPRTN
)

(provide (struct-out CPYFPRTN))

(define int->CPYFPRTRN/struct int->CPYFP/struct)

(define (int->CPYFPRTRN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #xa)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPRTRN (int->CPYFPRTRN/struct i))])
)

(define (CPYFPRTRN->int rcw)
  (match-define (CPYFPRTRN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPRTRN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPRTRN->int
  #:property prop:try-from-int int->CPYFPRTRN
)

(provide (struct-out CPYFPRTRN))

(define int->CPYFPRTWN/struct int->CPYFP/struct)

(define (int->CPYFPRTWN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #x6)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPRTWN (int->CPYFPRTWN/struct i))])
)

(define (CPYFPRTWN->int rcw)
  (match-define (CPYFPRTWN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPRTWN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPRTWN->int
  #:property prop:try-from-int int->CPYFPRTWN
)

(provide (struct-out CPYFPRTWN))

(define int->CPYFPT/struct int->CPYFP/struct)

(define (int->CPYFPT i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #x3)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPT (int->CPYFPT/struct i))])
)

(define (CPYFPT->int rcw)
  (match-define (CPYFPT sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPT (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPT->int
  #:property prop:try-from-int int->CPYFPT
)

(provide (struct-out CPYFPT))

(define int->CPYFPTN/struct int->CPYFP/struct)

(define (int->CPYFPTN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #xf)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPTN (int->CPYFPTN/struct i))])
)

(define (CPYFPTN->int rcw)
  (match-define (CPYFPTN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPTN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPTN->int
  #:property prop:try-from-int int->CPYFPTN
)

(provide (struct-out CPYFPTN))

(define int->CPYFPTRN/struct int->CPYFP/struct)

(define (int->CPYFPTRN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #xb)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPTRN (int->CPYFPTRN/struct i))])
)

(define (CPYFPTRN->int rcw)
  (match-define (CPYFPTRN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPTRN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPTRN->int
  #:property prop:try-from-int int->CPYFPTRN
)

(provide (struct-out CPYFPTRN))

(define int->CPYFPTWN/struct int->CPYFP/struct)

(define (int->CPYFPTWN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #x7)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPTWN (int->CPYFPTWN/struct i))])
)

(define (CPYFPTWN->int rcw)
  (match-define (CPYFPTWN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPTWN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPTWN->int
  #:property prop:try-from-int int->CPYFPTWN
)

(provide (struct-out CPYFPTWN))

(define int->CPYFPWN/struct int->CPYFP/struct)

(define (int->CPYFPWN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #x4)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPWN (int->CPYFPWN/struct i))])
)

(define (CPYFPWN->int rcw)
  (match-define (CPYFPWN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPWN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPWN->int
  #:property prop:try-from-int int->CPYFPWN
)

(provide (struct-out CPYFPWN))

(define int->CPYFPWT/struct int->CPYFP/struct)

(define (int->CPYFPWT i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #x1)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPWT (int->CPYFPWT/struct i))])
)

(define (CPYFPWT->int rcw)
  (match-define (CPYFPWT sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPWT (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPWT->int
  #:property prop:try-from-int int->CPYFPWT
)

(provide (struct-out CPYFPWT))

(define int->CPYFPWTN/struct int->CPYFP/struct)

(define (int->CPYFPWTN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #xd)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPWTN (int->CPYFPWTN/struct i))])
)

(define (CPYFPWTN->int rcw)
  (match-define (CPYFPWTN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPWTN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPWTN->int
  #:property prop:try-from-int int->CPYFPWTN
)

(provide (struct-out CPYFPWTN))

(define int->CPYFPWTRN/struct int->CPYFP/struct)

(define (int->CPYFPWTRN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #x9)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPWTRN (int->CPYFPWTRN/struct i))])
)

(define (CPYFPWTRN->int rcw)
  (match-define (CPYFPWTRN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPWTRN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPWTRN->int
  #:property prop:try-from-int int->CPYFPWTRN
)

(provide (struct-out CPYFPWTRN))

(define int->CPYFPWTWN/struct int->CPYFP/struct)

(define (int->CPYFPWTWN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #x5)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYFPWTWN (int->CPYFPWTWN/struct i))])
)

(define (CPYFPWTWN->int rcw)
  (match-define (CPYFPWTWN sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYFPWTWN (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYFPWTWN->int
  #:property prop:try-from-int int->CPYFPWTWN
)

(provide (struct-out CPYFPWTWN))

(define int->CPYP/struct int->CPYFP/struct)

(define (int->CPYP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x1)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 16) #x0)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply CPYP (int->CPYP/struct i))])
)

(define (CPYP->int rcw)
  (match-define (CPYP sz op1 rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift #x1 24)
    (arithmetic-shift op1 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct CPYP (sz op1 rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int CPYP->int
  #:property prop:try-from-int int->CPYP
)

(provide (struct-out CPYP))
