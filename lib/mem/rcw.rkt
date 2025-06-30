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
