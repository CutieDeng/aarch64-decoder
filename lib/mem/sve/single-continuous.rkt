#lang racket

(require "../../util/in-feature.rkt")
(require "../../util/into-int.rkt")
(require "../../util/try-from-int.rkt")

(define (int->LD1B/struct i)
  (list
    (bitwise-bit-field i 21 24)
    (bitwise-bit-field i 16 20)
    (bitwise-bit-field i 10 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LD1B i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 23 25) #x0)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x5)
  ) #f]
  [else (apply LD1B (int->LD1B/struct i))])
)

(define (LD1B->int ld1)
  (match-define (LD1B dtype imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x5 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1B (dtype imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature #f
  #:property prop:into-int LD1B->int
  #:property prop:try-from-int int->LD1B
)

(provide (struct-out LD1B))

(define int->LD1D/struct int->LD1B/struct)

(define (int->LD1D i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 21 25) #xf)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x5)
  ) #f]
  [else (apply LD1D (int->LD1D/struct i))])
)

(define (LD1D->int ld1)
  (match-define (LD1D dtype imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x5 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1D (dtype imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature #f
  #:property prop:into-int LD1D->int
  #:property prop:try-from-int int->LD1D
)

(provide (struct-out LD1D))

(define int->LD1H/struct int->LD1B/struct)

(define (int->LD1H i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 23 25) #x1)
    (not (equal? (bitwise-bit-field i 21 23) #x0))
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x5)
  ) #f]
  [else (apply LD1H (int->LD1H/struct i))])
)

(define (LD1H->int ld1)
  (match-define (LD1H dtype imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x5 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1H (dtype imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature #f
  #:property prop:into-int LD1H->int
  #:property prop:try-from-int int->LD1H
)

(provide (struct-out LD1H))

(define (int->LD1ROB/struct i)
  (list
    (bitwise-bit-field i 23 25)
    (bitwise-bit-field i 21 23)
    (bitwise-bit-field i 16 20)
    (bitwise-bit-field i 10 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LD1ROB i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 23 25) #x0)
    (equal? (bitwise-bit-field i 21 23) #x1)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x1)
  ) #f]
  [else (apply LD1ROB (int->LD1ROB/struct i))])
)

(define (LD1ROB->int ld1)
  (match-define (LD1ROB msz ssz imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift msz 23)
    (arithmetic-shift ssz 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x1 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1ROB (msz ssz imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature #f
  #:property prop:into-int LD1ROB->int
  #:property prop:try-from-int int->LD1ROB
)

(provide (struct-out LD1ROB))

(define int->LD1ROD/struct int->LD1ROB/struct)

(define (int->LD1ROD i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 23 25) #x3)
    (equal? (bitwise-bit-field i 21 23) #x1)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x1)
  ) #f]
  [else (apply LD1ROD (int->LD1ROD/struct i))])
)

(define (LD1ROD->int ld1)
  (match-define (LD1ROD msz ssz imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift msz 23)
    (arithmetic-shift ssz 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x1 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1ROD (msz ssz imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature 'FEAT_F64MM
  #:property prop:into-int LD1ROD->int
  #:property prop:try-from-int int->LD1ROD
)

(provide (struct-out LD1ROD))

(define int->LD1ROH/struct int->LD1ROB/struct)

(define (int->LD1ROH i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 23 25) #x1)
    (equal? (bitwise-bit-field i 21 23) #x1)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x1)
  ) #f]
  [else (apply LD1ROH (int->LD1ROH/struct i))])
)

(define (LD1ROH->int ld1)
  (match-define (LD1ROH msz ssz imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift msz 23)
    (arithmetic-shift ssz 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x1 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1ROH (msz ssz imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature 'FEAT_F64MM
  #:property prop:into-int LD1ROH->int
  #:property prop:try-from-int int->LD1ROH
)

(provide (struct-out LD1ROH))

(define int->LD1ROW/struct int->LD1ROB/struct)

(define (int->LD1ROW i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 23 25) #x2)
    (equal? (bitwise-bit-field i 21 23) #x1)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x1)
  ) #f]
  [else (apply LD1ROW (int->LD1ROW/struct i))])
)

(define (LD1ROW->int ld1)
  (match-define (LD1ROW msz ssz imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift msz 23)
    (arithmetic-shift ssz 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x1 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1ROW (msz ssz imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature 'FEAT_F64MM
  #:property prop:into-int LD1ROW->int
  #:property prop:try-from-int int->LD1ROW
)

(provide (struct-out LD1ROW))

(define int->LD1RQB/struct int->LD1ROB/struct)

(define (int->LD1RQB i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 23 25) #x0)
    (equal? (bitwise-bit-field i 21 23) #x0)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x1)
  ) #f]
  [else (apply LD1RQB (int->LD1RQB/struct i))])
)

(define (LD1RQB->int ld1)
  (match-define (LD1RQB msz ssz imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift msz 23)
    (arithmetic-shift ssz 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x1 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1RQB (msz ssz imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature '(or FEAT_SVE FEAT_F64MM)
  #:property prop:into-int LD1RQB->int
  #:property prop:try-from-int int->LD1RQB
)

(provide (struct-out LD1RQB))

(define int->LD1RQD/struct int->LD1ROB/struct)

(define (int->LD1RQD i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 23 25) #x3)
    (equal? (bitwise-bit-field i 21 23) #x0)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x1)
  ) #f]
  [else (apply LD1RQD (int->LD1RQD/struct i))])
)

(define (LD1RQD->int ld1)
  (match-define (LD1RQD msz ssz imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift msz 23)
    (arithmetic-shift ssz 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x1 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1RQD (msz ssz imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature '(or FEAT_SVE FEAT_F64MM)
  #:property prop:into-int LD1RQD->int
  #:property prop:try-from-int int->LD1RQD
)

(provide (struct-out LD1RQD))

(define int->LD1RQH/struct int->LD1ROB/struct)

(define (int->LD1RQH i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 23 25) #x1)
    (equal? (bitwise-bit-field i 21 23) #x0)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x1)
  ) #f]
  [else (apply LD1RQH (int->LD1RQH/struct i))])
)

(define (LD1RQH->int ld1)
  (match-define (LD1RQH msz ssz imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift msz 23)
    (arithmetic-shift ssz 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x1 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1RQH (msz ssz imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature '(or FEAT_SVE FEAT_F64MM)
  #:property prop:into-int LD1RQH->int
  #:property prop:try-from-int int->LD1RQH
)

(provide (struct-out LD1RQH))

(define int->LD1RQW/struct int->LD1ROB/struct)

(define (int->LD1RQW i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 23 25) #x2)
    (equal? (bitwise-bit-field i 21 23) #x0)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x1)
  ) #f]
  [else (apply LD1RQW (int->LD1RQW/struct i))])
)

(define (LD1RQW->int ld1)
  (match-define (LD1RQW msz ssz imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift msz 23)
    (arithmetic-shift ssz 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x1 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1RQW (msz ssz imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature '(or FEAT_SVE FEAT_F64MM)
  #:property prop:into-int LD1RQW->int
  #:property prop:try-from-int int->LD1RQW
)

(provide (struct-out LD1RQW))

(define (int->LD1SB/struct i)
  (list
    (bitwise-bit-field i 21 25)
    (bitwise-bit-field i 16 20)
    (bitwise-bit-field i 10 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LD1SB i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 21 25) #xe)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x5)
  ) #f]
  [else (apply LD1SB (int->LD1SB/struct i))])
)

(define (LD1SB->int ld1)
  (match-define (LD1SB dtype imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x5 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1SB (dtype imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature '(or FEAT_SVE FEAT_F64MM)
  #:property prop:into-int LD1SB->int
  #:property prop:try-from-int int->LD1SB
)

(provide (struct-out LD1SB))

(define int->LD1SW/struct int->LD1SB/struct)

(define (int->LD1SW i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 21 25) #x4)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x5)
  ) #f]
  [else (apply LD1SW (int->LD1SW/struct i))])
)

(define (LD1SW->int ld1)
  (match-define (LD1SW dtype imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x5 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1SW (dtype imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature '(or FEAT_SVE FEAT_F64MM)
  #:property prop:into-int LD1SW->int
  #:property prop:try-from-int int->LD1SW
)

(provide (struct-out LD1SW))

(define int->LD1W/struct int->LD1SB/struct)

(define (int->LD1W i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 21 25) #xa)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x5)
  ) #f]
  [else (apply LD1W (int->LD1W/struct i))])
)

(define (LD1W->int ld1)
  (match-define (LD1W dtype imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x5 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LD1W (dtype imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature '(or FEAT_SVE FEAT_F64MM)
  #:property prop:into-int LD1W->int
  #:property prop:try-from-int int->LD1W
)

(provide (struct-out LD1W))

(define (int->ST1B/struct i)
  (list
    (bitwise-bit-field i 23 25)
    (bitwise-bit-field i 21 23)
    (bitwise-bit-field i 16 20)
    (bitwise-bit-field i 10 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->ST1B i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x72)
    (equal? (bitwise-bit-field i 23 25) #x0)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x7)
  ) #f]
  [else (apply ST1B (int->ST1B/struct i))])
)

(define (ST1B->int ld1)
  (match-define (ST1B msz size imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x72 25)
    (arithmetic-shift msz 23)
    (arithmetic-shift size 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x7 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct ST1B (msz size imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature '(or FEAT_SVE FEAT_SME)
  #:property prop:into-int ST1B->int
  #:property prop:try-from-int int->ST1B
)

(provide (struct-out ST1B))

(define int->ST1D/struct int->ST1B/struct)

(define (int->ST1D i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x72)
    (equal? (bitwise-bit-field i 23 25) #x3)
    (equal? (bitwise-bit-field i 21 23) #x3)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x7)
  ) #f]
  [else (apply ST1D (int->ST1D/struct i))])
)

(define (ST1D->int ld1)
  (match-define (ST1D msz opc imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x72 25)
    (arithmetic-shift msz 23)
    (arithmetic-shift opc 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x7 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct ST1D (msz opc imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature '(or FEAT_SVE FEAT_SME)
  #:property prop:into-int ST1D->int
  #:property prop:try-from-int int->ST1D
)

(provide (struct-out ST1D))

(define int->ST1H/struct int->ST1B/struct)

(define (int->ST1H i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x72)
    (equal? (bitwise-bit-field i 23 25) #x1)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x7)
  ) #f]
  [else (apply ST1H (int->ST1H/struct i))])
)

(define (ST1H->int ld1)
  (match-define (ST1H msz size imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x72 25)
    (arithmetic-shift msz 23)
    (arithmetic-shift size 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x7 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct ST1H (msz size imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature '(or FEAT_SVE FEAT_SME)
  #:property prop:into-int ST1H->int
  #:property prop:try-from-int int->ST1H
)

(provide (struct-out ST1H))

(define (int->ST1W/struct i)
  (list
    (bitwise-bit-field i 23 25)
    (bitwise-bit-field i 21 22)
    (bitwise-bit-field i 16 20)
    (bitwise-bit-field i 10 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->ST1W i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x72)
    (equal? (bitwise-bit-field i 23 25) #x2)
    (equal? (bitwise-bit-field i 22 23) #x1)
    (equal? (bitwise-bit-field i 20 21) #x0)
    (equal? (bitwise-bit-field i 13 16) #x7)
  ) #f]
  [else (apply ST1W (int->ST1W/struct i))])
)

(define (ST1W->int ld1)
  (match-define (ST1W msz sz imm4 pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x72 25)
    (arithmetic-shift msz 23)
    (arithmetic-shift #x1 22)
    (arithmetic-shift sz 21)
    (arithmetic-shift imm4 16)
    (arithmetic-shift #x7 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct ST1W (msz sz imm4 pg rn zt)
  #:transparent
  #:property prop:in-feature '(or FEAT_SVE FEAT_SME)
  #:property prop:into-int ST1W->int
  #:property prop:try-from-int int->ST1W
)

(provide (struct-out ST1W))

(define (int->LDFF1B/b/struct i)
  (list
    (bitwise-bit-field i 21 25)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 10 13)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDFF1B/b i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 21 25) #x0)
    (equal? (bitwise-bit-field i 13 16) #x3)
  ) #f]
  [else (apply LDFF1B/b (int->LDFF1B/b/struct i))])
)

(define (LDFF1B/b->int ld1)
  (match-define (LDFF1B/b dtype rm pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift #x3 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LDFF1B/b (dtype rm pg rn zt)
  #:transparent
  #:property prop:in-feature 'FEAT_SVE
  #:property prop:into-int LDFF1B/b->int
  #:property prop:try-from-int int->LDFF1B/b
)

(provide (struct-out LDFF1B/b))

(define int->LDFF1B/h/struct int->LDFF1B/b/struct)

(define (int->LDFF1B/h i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 21 25) #x1)
    (equal? (bitwise-bit-field i 13 16) #x3)
  ) #f]
  [else (apply LDFF1B/h (int->LDFF1B/h/struct i))])
)

(define (LDFF1B/h->int ld1)
  (match-define (LDFF1B/h dtype rm pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift #x3 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LDFF1B/h (dtype rm pg rn zt)
  #:transparent
  #:property prop:in-feature 'FEAT_SVE
  #:property prop:into-int LDFF1B/h->int
  #:property prop:try-from-int int->LDFF1B/h
)

(provide (struct-out LDFF1B/h))

(define int->LDFF1B/w/struct int->LDFF1B/b/struct)

(define (int->LDFF1B/w i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 21 25) #x2)
    (equal? (bitwise-bit-field i 13 16) #x3)
  ) #f]
  [else (apply LDFF1B/w (int->LDFF1B/w/struct i))])
)

(define (LDFF1B/w->int ld1)
  (match-define (LDFF1B/w dtype rm pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift #x3 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LDFF1B/w (dtype rm pg rn zt)
  #:transparent
  #:property prop:in-feature 'FEAT_SVE
  #:property prop:into-int LDFF1B/w->int
  #:property prop:try-from-int int->LDFF1B/w
)

(provide (struct-out LDFF1B/w))

(define int->LDFF1B/d/struct int->LDFF1B/b/struct)

(define (int->LDFF1B/d i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) #x52)
    (equal? (bitwise-bit-field i 21 25) #x3)
    (equal? (bitwise-bit-field i 13 16) #x3)
  ) #f]
  [else (apply LDFF1B/d (int->LDFF1B/d/struct i))])
)

(define (LDFF1B/d->int ld1)
  (match-define (LDFF1B/d dtype rm pg rn zt) ld1)
  (bitwise-ior
    (arithmetic-shift #x52 25)
    (arithmetic-shift dtype 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift #x3 13)
    (arithmetic-shift pg 10)
    (arithmetic-shift rn 5)
    zt
  )
)

(struct LDFF1B/d (dtype rm pg rn zt)
  #:transparent
  #:property prop:in-feature 'FEAT_SVE
  #:property prop:into-int LDFF1B/d->int
  #:property prop:try-from-int int->LDFF1B/d
)

(provide (struct-out LDFF1B/d))
