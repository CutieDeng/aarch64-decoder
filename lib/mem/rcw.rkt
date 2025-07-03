#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->RCWCAS/struct i)
  (list
    (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 23 24)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->RCWCAS i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 16) #x2)
  ) #f]
  [else (apply RCWCAS (int->RCWCAS/struct i))])
)

(define (RCWCAS->int rcw)
  (match-define (RCWCAS s a r rs rn rt) rcw)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWCAS (s a r rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_THE . #t))
  #:property prop:into-int RCWCAS->int
  #:property prop:try-from-int int->RCWCAS
)

(provide (struct-out RCWCAS))

(define int->RCWSCAS/struct int->RCWCAS/struct)

(define (int->RCWSCAS i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x1)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 16) #x2)
  ) #f]
  [else (apply RCWSCAS (int->RCWSCAS/struct i))])
)

(define (RCWSCAS->int rcw)
  (match-define (RCWSCAS s a r rs rn rt) rcw)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWSCAS (s a r rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_THE . #t))
  #:property prop:into-int RCWSCAS->int
  #:property prop:try-from-int int->RCWSCAS
)

(provide (struct-out RCWSCAS))

(define (int->RCWCLR/struct i)
  (list
    (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 23 24)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 12 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->RCWCLR i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) #x1)
    (equal? (bitwise-bit-field i 10 12) #x0)
  ) #f]
  [else (apply RCWCLR (int->RCWCLR/struct i))])
)

(define (RCWCLR->int rcw)
  (match-define (RCWCLR s a r rs o3 opc rn rt) rcw)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift o3 15)
    (arithmetic-shift opc 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWCLR (s a r rs o3 opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_THE . #t))
  #:property prop:into-int RCWCLR->int
  #:property prop:try-from-int int->RCWCLR
)

(provide (struct-out RCWCLR))

(define int->RCWSCLR/struct int->RCWCLR/struct)

(define (int->RCWSCLR i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) #x1)
    (equal? (bitwise-bit-field i 10 12) #x0)
  ) #f]
  [else (apply RCWSCLR (int->RCWSCLR/struct i))])
)

(define (RCWSCLR->int rcw)
  (match-define (RCWSCLR s a r rs o3 opc rn rt) rcw)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift o3 15)
    (arithmetic-shift opc 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWSCLR (s a r rs o3 opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_THE . #t))
  #:property prop:into-int RCWSCLR->int
  #:property prop:try-from-int int->RCWSCLR
)

(provide (struct-out RCWSCLR))

(define int->RCWSET/struct int->RCWCLR/struct)

(define (int->RCWSET i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) #x3)
    (equal? (bitwise-bit-field i 10 12) #x0)
  ) #f]
  [else (apply RCWSET (int->RCWSET/struct i))])
)

(define (RCWSET->int rcw)
  (match-define (RCWSET s a r rs o3 opc rn rt) rcw)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift o3 15)
    (arithmetic-shift opc 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWSET (s a r rs o3 opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_THE . #t))
  #:property prop:into-int RCWSET->int
  #:property prop:try-from-int int->RCWSET
)

(provide (struct-out RCWSET))

(define int->RCWSWP/struct int->RCWCLR/struct)

(define (int->RCWSWP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) #x2)
    (equal? (bitwise-bit-field i 10 12) #x0)
  ) #f]
  [else (apply RCWSWP (int->RCWSWP/struct i))])
)

(define (RCWSWP->int rcw)
  (match-define (RCWSWP s a r rs o3 opc rn rt) rcw)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift o3 15)
    (arithmetic-shift opc 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWSWP (s a r rs o3 opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_THE . #t))
  #:property prop:into-int RCWSWP->int
  #:property prop:try-from-int int->RCWSWP
)

(provide (struct-out RCWSWP))

(define int->RCWSSWP/struct int->RCWCLR/struct)

(define (int->RCWSSWP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) #x2)
    (equal? (bitwise-bit-field i 10 12) #x0)
  ) #f]
  [else (apply RCWSSWP (int->RCWSSWP/struct i))])
)

(define (RCWSSWP->int rcw)
  (match-define (RCWSSWP s a r rs o3 opc rn rt) rcw)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift o3 15)
    (arithmetic-shift opc 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWSSWP (s a r rs o3 opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_THE . #t))
  #:property prop:into-int RCWSSWP->int
  #:property prop:try-from-int int->RCWSSWP
)

(provide (struct-out RCWSSWP))

(define int->RCWCASP/struct int->RCWCAS/struct)

(define (int->RCWCASP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 16) #x3)
  ) #f]
  [else (apply RCWCASP (int->RCWCASP/struct i))])
)

(define (RCWCASP->int rcw)
  (match-define (RCWCASP s a r rs rn rt) rcw)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWCASP (s a r rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_THE . #t) (FEAT_D128 . #t))
  #:property prop:into-int RCWCASP->int
  #:property prop:try-from-int int->RCWCASP
)

(provide (struct-out RCWCASP))

(define int->RCWSCASP/struct int->RCWCAS/struct)

(define (int->RCWSCASP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x1)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 16) #x3)
  ) #f]
  [else (apply RCWSCASP (int->RCWSCASP/struct i))])
)

(define (RCWSCASP->int rcw)
  (match-define (RCWSCASP s a r rs rn rt) rcw)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWSCASP (s a r rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_THE . #t) (FEAT_D128 . #t))
  #:property prop:into-int RCWSCASP->int
  #:property prop:try-from-int int->RCWSCASP
)

(provide (struct-out RCWSCASP))

(define int->RCWCLRP/struct int->RCWCLR/struct)

(define (int->RCWCLRP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) #x1)
    (equal? (bitwise-bit-field i 12 15) #x1)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply RCWCLRP (int->RCWCLRP/struct i))])
)

(define (RCWCLRP->int rcw)
  (match-define (RCWCLRP s a r rt2 o3 opc rn rt) rcw)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rt2 16)
    (arithmetic-shift o3 15)
    (arithmetic-shift opc 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWCLRP (s a r rt2 o3 opc rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_THE . #t) (FEAT_D128 . #t))
  #:property prop:into-int RCWCLRP->int
  #:property prop:try-from-int int->RCWCLRP
)

(provide (struct-out RCWCLRP))

(define int->RCWSCLRP/struct int->RCWCLR/struct)

(define (int->RCWSCLRP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x1)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) #x1)
    (equal? (bitwise-bit-field i 12 15) #x1)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply RCWSCLRP (int->RCWSCLRP/struct i))])
)

(define (RCWSCLRP->int rcw)
  (match-define (RCWSCLRP s a r rs rn rt) rcw)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWSCLRP (s a r rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_THE . #t) (FEAT_D128 . #t))
  #:property prop:into-int RCWSCLRP->int
  #:property prop:try-from-int int->RCWSCLRP
)

(provide (struct-out RCWSCLRP))

(define int->RCWSETP/struct int->RCWCLR/struct)

(define (int->RCWSETP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 21 22) #x1)
    (equal? (bitwise-bit-field i 15 16) #x1)
    (equal? (bitwise-bit-field i 12 15) #x3)
    (equal? (bitwise-bit-field i 10 12) #x0)
  ) #f]
  [else (apply RCWSETP (int->RCWSETP/struct i))])
)

(define (RCWSETP->int rcw)
  (match-define (RCWSETP s a r rt2 o3 opc rn rt) rcw)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rt2 16)
    (arithmetic-shift o3 15)
    (arithmetic-shift opc 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWSETP (s a r rt2 o3 opc rn rt)
  #:transparent
  #:property prop:in-feature '(and FEAT_D128 FEAT_THE)
  #:property prop:into-int RCWSETP->int
  #:property prop:try-from-int int->RCWSETP
)

(provide (struct-out RCWSETP))
