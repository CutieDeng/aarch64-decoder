#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define SYS-head #x354)

(define (int->SYS/struct i)
  (list (bitwise-bit-field i 16 18)
    (bitwise-bit-field i 12 16)
    (bitwise-bit-field i 8 12)
    (bitwise-bit-field i 5 8)
    (bitwise-bit-field i 0 5)
  )
)

(define (int->SYS i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 22 32) SYS-head)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 19 21) #x1)
    ) #f]
    [else (apply SYS (int->SYS/struct i))])
)

(define (SYS->int e)
  (match-define (SYS op1 crn crm op2 rt) e)
  (bitwise-ior
    (arithmetic-shift SYS-head 22)
    (arithmetic-shift #x1 19)
    (arithmetic-shift op1 16)
    (arithmetic-shift crn 12)
    (arithmetic-shift crm 8)
    (arithmetic-shift op2 5)
    rt
  )
)

(struct SYS (op1 crn crm op2 rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_WFxT . #t))
  #:property prop:into-int SYS->int
  #:property prop:try-from-int int->SYS
)

(provide (struct-out SYS))