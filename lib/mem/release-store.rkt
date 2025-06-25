#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->STLR/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 10 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STLR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 24 30) #x8)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply STLR (int->STLR/struct i))])
)

(define (STLR->int stlr)
  (match-define (STLR size l rs o0 rt2 rn rt) stlr)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x8 24)
    (arithmetic-shift #x1 23)
    (arithmetic-shift l 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STLR (size l rs o0 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STLR->int
  #:property prop:try-from-int int->STLR
)

(provide (struct-out STLR))

(define (int->STLR/Pre/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STLR/Pre i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 23 26) #x3)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 10 22) #x2)
  ) #f]
  [else (apply STLR/Pre (int->STLR/Pre/struct i))])
)

(define (STLR/Pre->int stlr)
  (match-define (STLR/Pre size rn rt) stlr)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x3 23)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STLR/Pre (size rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC3 . #t))
  #:property prop:into-int STLR/Pre->int
  #:property prop:try-from-int int->STLR/Pre
)

(provide (struct-out STLR/Pre))

(define int->STLRB/struct int->STLR/struct)

(define (int->STLRB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 24 30) #x8)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply STLRB (int->STLRB/struct i))])
)

(define (STLRB->int stlr)
  (match-define (STLRB size l rs o0 rt2 rn rt) stlr)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x8 24)
    (arithmetic-shift #x1 23)
    (arithmetic-shift l 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STLRB (size l rs o0 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STLRB->int
  #:property prop:try-from-int int->STLRB
)

(provide (struct-out STLRB))

(define int->STLRH/struct int->STLR/struct)

(define (int->STLRH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 24 30) #x8)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply STLRH (int->STLRH/struct i))])
)

(define (STLRH->int stlr)
  (match-define (STLRH size l rs o0 rt2 rn rt) stlr)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x8 24)
    (arithmetic-shift #x1 23)
    (arithmetic-shift l 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STLRH (size l rs o0 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STLRH->int
  #:property prop:try-from-int int->STLRH
)

(provide (struct-out STLRH))

(define (int->STLUR/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STLUR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 22 24) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply STLUR (int->STLUR/struct i))])
)

(define (STLUR->int stlr)
  (match-define (STLUR size opc imm9 rn rt) stlr)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STLUR (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC2 . #t))
  #:property prop:into-int STLUR->int
  #:property prop:try-from-int int->STLUR
)

(provide (struct-out STLUR))

(define int->STLURB/struct int->STLUR/struct)

(define (int->STLURB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 22 24) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply STLURB (int->STLURB/struct i))])
)

(define (STLURB->int stlr)
  (match-define (STLURB size opc imm9 rn rt) stlr)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STLURB (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC2 . #t))
  #:property prop:into-int STLURB->int
  #:property prop:try-from-int int->STLURB
)

(provide (struct-out STLURB))

(define int->STLURH/struct int->STLUR/struct)

(define (int->STLURH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 22 24) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply STLURH (int->STLURH/struct i))])
)

(define (STLURH->int stlr)
  (match-define (STLURH size opc imm9 rn rt) stlr)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STLURH (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC2 . #t))
  #:property prop:into-int STLURH->int
  #:property prop:try-from-int int->STLURH
)

(provide (struct-out STLURH))
