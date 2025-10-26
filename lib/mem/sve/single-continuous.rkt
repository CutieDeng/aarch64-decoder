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
