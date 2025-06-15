#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define ERET-head #x6b)

(define ERET-literal (bitwise-ior
  (arithmetic-shift ERET-head 25)
  (arithmetic-shift #x4 21)
  (arithmetic-shift #x1f 16)
  (arithmetic-shift #x1f 5)
))

(define (int->ERET i)
  (cond [(nand (equal? i ERET-literal)) #f]
    [else (ERET)])
)

(define (ERET->int _e)
  ERET-literal
)

(struct ERET ()
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ERET->int
  #:property prop:try-from-int int->ERET
)

(provide (struct-out ERET))

(define (int->ERETAA/struct i)
  (list (bitwise-bit-field i 10 11))
)

(define ERETAA-head #x6b)

(define (int->ERETAA i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) ERETAA-head)
    (equal? (bitwise-bit-field i 21 25) #x4)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 12 16) 0)
    (equal? (bitwise-bit-field i 11 12) 1)
    (equal? (bitwise-bit-field i 5 10) #x1f)
    (equal? (bitwise-bit-field i 0 5) #x1f)
    ) #f]
    [else (apply ERETAA (int->ERETAA/struct i))])
)

(define (ERETAA->int e)
  (match-define (ERETAA m) e)
  (bitwise-ior
    (arithmetic-shift ERETAA-head 25)
    (arithmetic-shift #x4 21)
    (arithmetic-shift #x1f 16)
    (arithmetic-shift 1 11)
    (arithmetic-shift m 10)
    (arithmetic-shift #x1f 5)
    #x1f
  )
)

(struct ERETAA (m)
  #:transparent
  #:property prop:in-feature #hash((FEAT_PAuth . #t))
  #:property prop:into-int ERETAA->int
  #:property prop:try-from-int int->ERETAA
)

(provide (struct-out ERETAA))

(define (int->BRK/struct i)
  (list (bitwise-bit-field i 5 21))
)

(define BRK-head #xd4)

(define (int->BRK i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 24 32) BRK-head)
    (equal? (bitwise-bit-field i 21 24) #x1)
    (equal? (bitwise-bit-field i 2 5) 0)
    (equal? (bitwise-bit-field i 0 2) 0)
    ) #f]
    [else (apply BRK (int->BRK/struct i))])
)

(define (BRK->int b)
  (match-define (BRK imm) b)
  (bitwise-ior
    (arithmetic-shift BRK-head 24)
    (arithmetic-shift #x1 21)
    (arithmetic-shift imm 5)
  )
)

(struct BRK (imm16)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int BRK->int
  #:property prop:try-from-int int->BRK
)

(provide (struct-out BRK))

(define int->HLT/struct int->BRK/struct)

(define HLT-head BRK-head)

(define (int->HLT i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 24 32) HLT-head)
    (equal? (bitwise-bit-field i 21 24) #x2)
    (equal? (bitwise-bit-field i 2 5) 0)
    (equal? (bitwise-bit-field i 0 2) 0)
    ) #f]
    [else (apply HLT (int->HLT/struct i))])
)

(define (HLT->int b)
  (match-define (HLT imm) b)
  (bitwise-ior
    (arithmetic-shift HLT-head 24)
    (arithmetic-shift #x2 21)
    (arithmetic-shift imm 5)
  )
)

(struct HLT (imm16)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int HLT->int
  #:property prop:try-from-int int->HLT
)

(provide (struct-out HLT))

(define int->HVC/struct int->BRK/struct)

(define HVC-head BRK-head)

(define (int->HVC i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 24 32) HVC-head)
    (equal? (bitwise-bit-field i 21 24) #x0)
    (equal? (bitwise-bit-field i 2 5) 0)
    (equal? (bitwise-bit-field i 0 2) 2)
    ) #f]
    [else (apply HVC (int->HVC/struct i))])
)

(define (HVC->int b)
  (match-define (HVC imm) b)
  (bitwise-ior
    (arithmetic-shift HVC-head 24)
    (arithmetic-shift imm 5)
    #x2
  )
)

(struct HVC (imm16)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int HVC->int
  #:property prop:try-from-int int->HVC
)

(provide (struct-out HVC))

(define int->SMC/struct int->BRK/struct)

(define SMC-head BRK-head)

(define (int->SMC i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 24 32) SMC-head)
    (equal? (bitwise-bit-field i 21 24) #x0)
    (equal? (bitwise-bit-field i 2 5) 0)
    (equal? (bitwise-bit-field i 0 2) 3)
    ) #f]
    [else (apply SMC (int->SMC/struct i))])
)

(define (SMC->int b)
  (match-define (SMC imm) b)
  (bitwise-ior
    (arithmetic-shift SMC-head 24)
    (arithmetic-shift imm 5)
    #x3
  )
)

(struct SMC (imm16)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int SMC->int
  #:property prop:try-from-int int->SMC
)

(provide (struct-out SMC))

(define int->SVC/struct int->BRK/struct)

(define SVC-head BRK-head)

(define (int->SVC i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 24 32) SVC-head)
    (equal? (bitwise-bit-field i 21 24) #x0)
    (equal? (bitwise-bit-field i 2 5) 0)
    (equal? (bitwise-bit-field i 0 2) 1)
    ) #f]
    [else (apply SVC (int->SVC/struct i))])
)

(define (SVC->int b)
  (match-define (SVC imm) b)
  (bitwise-ior
    (arithmetic-shift SVC-head 24)
    (arithmetic-shift imm 5)
    #x1
  )
)

(struct SVC (imm16)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int SVC->int
  #:property prop:try-from-int int->SVC
)

(provide (struct-out SVC))

(define int->DCPS1/struct int->BRK/struct)

(define DCPS1-head BRK-head)

(define (int->DCPS1 i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 24 32) DCPS1-head)
    (equal? (bitwise-bit-field i 21 24) #x5)
    (equal? (bitwise-bit-field i 2 5) 0)
    (equal? (bitwise-bit-field i 0 2) 1)
    ) #f]
    [else (apply DCPS1 (int->DCPS1/struct i))])
)

(define (DCPS1->int b)
  (match-define (DCPS1 imm) b)
  (bitwise-ior
    (arithmetic-shift DCPS1-head 24)
    (arithmetic-shift #x5 21)
    (arithmetic-shift imm 5)
    #x1
  )
)

(struct DCPS1 (imm16)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int DCPS1->int
  #:property prop:try-from-int int->DCPS1
)

(provide (struct-out DCPS1))

(define int->DCPS2/struct int->BRK/struct)

(define DCPS2-head BRK-head)

(define (int->DCPS2 i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 24 32) DCPS2-head)
    (equal? (bitwise-bit-field i 21 24) #x5)
    (equal? (bitwise-bit-field i 2 5) 0)
    (equal? (bitwise-bit-field i 0 2) 2)
    ) #f]
    [else (apply DCPS2 (int->DCPS2/struct i))])
)

(define (DCPS2->int b)
  (match-define (DCPS2 imm) b)
  (bitwise-ior
    (arithmetic-shift DCPS2-head 24)
    (arithmetic-shift #x5 21)
    (arithmetic-shift imm 5)
    #x2
  )
)

(struct DCPS2 (imm16)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int DCPS2->int
  #:property prop:try-from-int int->DCPS2
)

(provide (struct-out DCPS2))

(define int->DCPS3/struct int->BRK/struct)

(define DCPS3-head BRK-head)

(define (int->DCPS3 i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 24 32) DCPS3-head)
    (equal? (bitwise-bit-field i 21 24) #x5)
    (equal? (bitwise-bit-field i 2 5) 0)
    (equal? (bitwise-bit-field i 0 2) 3)
    ) #f]
    [else (apply DCPS3 (int->DCPS3/struct i))])
)

(define (DCPS3->int b)
  (match-define (DCPS3 imm) b)
  (bitwise-ior
    (arithmetic-shift DCPS3-head 24)
    (arithmetic-shift #x5 21)
    (arithmetic-shift imm 5)
    #x3
  )
)

(struct DCPS3 (imm16)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int DCPS3->int
  #:property prop:try-from-int int->DCPS3
)

(provide (struct-out DCPS3))

(define DRPS-head ERET-head)

(define DRPS-literal (bitwise-ior
  (arithmetic-shift DRPS-head 25)
  (arithmetic-shift #x5 21)
  (arithmetic-shift #x1f 16)
  (arithmetic-shift #x1f 5)
))

(define (int->DRPS i)
  (cond [(nand (equal? i DRPS-literal)) #f]
    [else (DRPS)])
)

(define (DRPS->int _e)
  DRPS-literal
)

(struct DRPS ()
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int DRPS->int
  #:property prop:try-from-int int->DRPS
)

(provide (struct-out DRPS))