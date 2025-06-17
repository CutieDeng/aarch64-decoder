#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->MRS/struct i)
  (list (bitwise-bit-field i 19 20) 
    (bitwise-bit-field i 16 19)
    (bitwise-bit-field i 12 16)
    (bitwise-bit-field i 8 12)
    (bitwise-bit-field i 5 8)
    (bitwise-bit-field i 0 5)
    )
)

(define MRS-head #x354)

(define (int->MRS i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 22 32) MRS-head)
    (equal? (bitwise-bit-field i 21 22) #x1)
    (equal? (bitwise-bit-field i 20 21) #x1)
    ) #f]
    [else (apply MRS (int->MRS/struct i))])
)

(define (MRS->int e)
  (match-define (MRS o0 op1 crn crm op2 rt) e)
  (bitwise-ior
    (arithmetic-shift MRS-head 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift #x1 20)
    (arithmetic-shift o0 19)
    (arithmetic-shift op1 16)
    (arithmetic-shift crn 12)
    (arithmetic-shift crm 8)
    (arithmetic-shift op2 5)
    rt
  )
)

(struct MRS (o0 op1 crn crm op2 rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int MRS->int
  #:property prop:try-from-int int->MRS
)

(provide (struct-out MRS))

(define int->MSR/r/struct int->MRS/struct)

(define MSR/r-head MRS-head)

(define (int->MSR/r i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 22 32) MSR/r-head)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 20 21) #x1)
    ) #f]
    [else (apply MSR/r (int->MSR/r/struct i))])
)

(define (MSR/r->int e)
  (match-define (MSR/r o0 op1 crn crm op2 rt) e)
  (bitwise-ior
    (arithmetic-shift MSR/r-head 22)
    (arithmetic-shift #x1 20)
    (arithmetic-shift o0 19)
    (arithmetic-shift op1 16)
    (arithmetic-shift crn 12)
    (arithmetic-shift crm 8)
    (arithmetic-shift op2 5)
    rt
  )
)

(struct MSR/r (o0 op1 crn crm op2 rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int MSR/r->int
  #:property prop:try-from-int int->MSR/r
)

(provide (struct-out MSR/r))

(define (int->MSR/pstate/struct i)
  (list (bitwise-bit-field i 16 19)
    (bitwise-bit-field i 8 12)
    (bitwise-bit-field i 5 8))
)

(define MSR/pstate-head MRS-head)

(define (int->MSR/pstate i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 22 32) MSR/pstate-head)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 19 21) #x0)
    (equal? (bitwise-bit-field i 12 16) #x4)
    (equal? (bitwise-bit-field i 0 5) #x1f)
    ) #f]
    [else (apply MSR/pstate (int->MSR/pstate/struct i))])
)

(define (MSR/pstate->int e)
  (match-define (MSR/pstate op1 crm op2) e)
  (bitwise-ior
    (arithmetic-shift MSR/pstate-head 22)
    (arithmetic-shift op1 16)
    (arithmetic-shift #x4 12)
    (arithmetic-shift crm 8)
    (arithmetic-shift op2 5)
    #x1f
  )
)

(struct MSR/pstate (op1 crm op2)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int MSR/pstate->int
  #:property prop:try-from-int int->MSR/pstate
)

(provide (struct-out MSR/pstate))

(define int->MRRS/struct int->MRS/struct)

(define MRRS-head #x355)

(define (int->MRRS i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 22 32) MRRS-head)
    (equal? (bitwise-bit-field i 21 22) #x1)
    (equal? (bitwise-bit-field i 20 21) #x1)
    ) #f]
    [else (apply MRRS (int->MRRS/struct i))])
)

(define (MRRS->int e)
  (match-define (MRRS o0 op1 crn crm op2 rt) e)
  (bitwise-ior
    (arithmetic-shift MRRS-head 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift #x1 20)
    (arithmetic-shift o0 19)
    (arithmetic-shift op1 16)
    (arithmetic-shift crn 12)
    (arithmetic-shift crm 8)
    (arithmetic-shift op2 5)
    rt
  )
)

(struct MRRS (o0 op1 crn crm op2 rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_SYSREG128 . #t))
  #:property prop:into-int MRRS->int
  #:property prop:try-from-int int->MRRS
)

(provide (struct-out MRRS))

(define int->MSRR/struct int->MRS/struct)

(define MSRR-head MRRS-head)

(define (int->MSRR i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 22 32) MSRR-head)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 20 21) #x1)
    ) #f]
    [else (apply MSRR (int->MSRR/struct i))])
)

(define (MSRR->int e)
  (match-define (MSRR o0 op1 crn crm op2 rt) e)
  (bitwise-ior
    (arithmetic-shift MSRR-head 22)
    (arithmetic-shift #x1 20)
    (arithmetic-shift o0 19)
    (arithmetic-shift op1 16)
    (arithmetic-shift crn 12)
    (arithmetic-shift crm 8)
    (arithmetic-shift op2 5)
    rt
  )
)

(struct MSRR (o0 op1 crn crm op2 rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_SYSREG128 . #t))
  #:property prop:into-int MSRR->int
  #:property prop:try-from-int int->MSRR
)

(provide (struct-out MSRR))