#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->ADDG/struct i)
  (list
    (bitwise-bit-field i 16 22)
    (bitwise-bit-field i 14 16)
    (bitwise-bit-field i 10 14)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->ADDG i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x1)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 29 30) #x0)
    (equal? (bitwise-bit-field i 23 29) #x23)
    (equal? (bitwise-bit-field i 22 23) #x0)
    (equal? (bitwise-bit-field i 14 16) #x0)
  ) #f]
  [else (apply ADDG (int->ADDG/struct i))])
)

(define (ADDG->int rcw)
  (match-define (ADDG uimm6 op3 uimm4 xn xd) rcw)
  (bitwise-ior
    (arithmetic-shift 1 31)
    (arithmetic-shift #x23 23)
    (arithmetic-shift uimm6 16)
    (arithmetic-shift op3 14)
    (arithmetic-shift uimm4 10)
    (arithmetic-shift xn 5)
    xd
  )
)

(struct ADDG (uimm6 op3 uimm4 xn xd)
  #:transparent
  #:property prop:in-feature 'FEAT_MTE
  #:property prop:into-int ADDG->int
  #:property prop:try-from-int int->ADDG
)

(provide (struct-out ADDG))

(define (int->GMI/struct i)
  (list
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->GMI i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x1)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 29 30) #x0)
    (equal? (bitwise-bit-field i 21 29) #xd6)
    (equal? (bitwise-bit-field i 15 16) #x0)
    (equal? (bitwise-bit-field i 14 15) #x0)
    (equal? (bitwise-bit-field i 13 14) #x0)
    (equal? (bitwise-bit-field i 12 13) #x1)
    (equal? (bitwise-bit-field i 11 12) #x0)
    (equal? (bitwise-bit-field i 10 11) #x1)
  ) #f]
  [else (apply GMI (int->GMI/struct i))])
)

(define (GMI->int rcw)
  (match-define (GMI xm xn xd) rcw)
  (bitwise-ior
    (arithmetic-shift 1 31)
    (arithmetic-shift #xd6 21)
    (arithmetic-shift xm 16)
    (arithmetic-shift #x1 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift xn 5)
    xd
  )
)

(struct GMI (xm xn xd)
  #:transparent
  #:property prop:in-feature 'FEAT_MTE
  #:property prop:into-int GMI->int
  #:property prop:try-from-int int->GMI
)

(provide (struct-out GMI))

(define int->IRG/struct int->GMI/struct)

(define (int->IRG i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x1)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 29 30) #x0)
    (equal? (bitwise-bit-field i 21 29) #xd6)
    (equal? (bitwise-bit-field i 15 16) #x0)
    (equal? (bitwise-bit-field i 14 15) #x0)
    (equal? (bitwise-bit-field i 13 14) #x0)
    (equal? (bitwise-bit-field i 12 13) #x1)
    (equal? (bitwise-bit-field i 11 12) #x0)
    (equal? (bitwise-bit-field i 10 11) #x0)
  ) #f]
  [else (apply IRG (int->IRG/struct i))])
)

(define (IRG->int rcw)
  (match-define (IRG xm xn xd) rcw)
  (bitwise-ior
    (arithmetic-shift 1 31)
    (arithmetic-shift #xd6 21)
    (arithmetic-shift xm 16)
    (arithmetic-shift #x1 12)
    (arithmetic-shift xn 5)
    xd
  )
)

(struct IRG (xm xn xd)
  #:transparent
  #:property prop:in-feature 'FEAT_MTE
  #:property prop:into-int IRG->int
  #:property prop:try-from-int int->IRG
)

(provide (struct-out IRG))

