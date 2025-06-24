#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LDAPR/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDAPR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) #x4)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDAPR (int->LDAPR/struct i))])
)

(define (LDAPR->int l)
  (match-define (LDAPR size rs rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift #x1 23)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift #x1 15)
    (arithmetic-shift #x4 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAPR (size rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC . #t))
  #:property prop:into-int LDAPR->int
  #:property prop:try-from-int int->LDAPR
)

(provide (struct-out LDAPR))

(define (int->LDAPR/Post/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDAPR/Post i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 23 26) #x3)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 10 22) #x2)
  ) #f]
  [else (apply LDAPR/Post (int->LDAPR/Post/struct i))])
)

(define (LDAPR/Post->int l)
  (match-define (LDAPR/Post size rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x3 23)
    (arithmetic-shift #x1 22)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAPR/Post (size rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC3 . #t))
  #:property prop:into-int LDAPR/Post->int
  #:property prop:try-from-int int->LDAPR/Post
)

(provide (struct-out LDAPR/Post))

(define int->LDAPRB/struct int->LDAPR/struct)

(define (int->LDAPRB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) #x4)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDAPRB (int->LDAPRB/struct i))])
)

(define (LDAPRB->int l)
  (match-define (LDAPRB size rs rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift #x1 23)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift #x1 15)
    (arithmetic-shift #x4 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAPRB (size rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC . #t))
  #:property prop:into-int LDAPRB->int
  #:property prop:try-from-int int->LDAPRB
)

(provide (struct-out LDAPRB))

(define int->LDAPRH/struct int->LDAPR/struct)

(define (int->LDAPRH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 27 30) #x7)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 24 26) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 12 15) #x4)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDAPRH (int->LDAPRH/struct i))])
)

(define (LDAPRH->int l)
  (match-define (LDAPRH size rs rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x7 27)
    (arithmetic-shift #x1 23)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift #x1 15)
    (arithmetic-shift #x4 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAPRH (size rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC . #t))
  #:property prop:into-int LDAPRH->int
  #:property prop:try-from-int int->LDAPRH
)

(provide (struct-out LDAPRH))

(define (int->LDAPUR/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 24)
    (bitwise-bit-field i 12 21)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDAPUR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDAPUR (int->LDAPUR/struct i))])
)

(define (LDAPUR->int l)
  (match-define (LDAPUR size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAPUR (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC2 . #t))
  #:property prop:into-int LDAPUR->int
  #:property prop:try-from-int int->LDAPUR
)

(provide (struct-out LDAPUR))

(define int->LDAPURB/struct int->LDAPUR/struct)

(define (int->LDAPURB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDAPURB (int->LDAPURB/struct i))])
)

(define (LDAPURB->int l)
  (match-define (LDAPURB size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAPURB (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC2 . #t))
  #:property prop:into-int LDAPURB->int
  #:property prop:try-from-int int->LDAPURB
)

(provide (struct-out LDAPURB))

(define int->LDAPURH/struct int->LDAPUR/struct)

(define (int->LDAPURH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 22 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDAPURH (int->LDAPURH/struct i))])
)

(define (LDAPURH->int l)
  (match-define (LDAPURH size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAPURH (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC2 . #t))
  #:property prop:into-int LDAPURH->int
  #:property prop:try-from-int int->LDAPURH
)

(provide (struct-out LDAPURH))

(define int->LDAPURSB/struct int->LDAPUR/struct)

(define (int->LDAPURSB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDAPURSB (int->LDAPURSB/struct i))])
)

(define (LDAPURSB->int l)
  (match-define (LDAPURSB size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAPURSB (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC2 . #t))
  #:property prop:into-int LDAPURSB->int
  #:property prop:try-from-int int->LDAPURSB
)

(provide (struct-out LDAPURSB))

(define int->LDAPURSH/struct int->LDAPUR/struct)

(define (int->LDAPURSH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDAPURSH (int->LDAPURSH/struct i))])
)

(define (LDAPURSH->int l)
  (match-define (LDAPURSH size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAPURSH (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC2 . #t))
  #:property prop:into-int LDAPURSH->int
  #:property prop:try-from-int int->LDAPURSH
)

(provide (struct-out LDAPURSH))

(define int->LDAPURSW/struct int->LDAPUR/struct)

(define (int->LDAPURSW i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) #x2)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 22 24) #x2)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 10 12) 0)
  ) #f]
  [else (apply LDAPURSW (int->LDAPURSW/struct i))])
)

(define (LDAPURSW->int l)
  (match-define (LDAPURSW size opc imm9 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift opc 22)
    (arithmetic-shift imm9 12)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAPURSW (size opc imm9 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LRCPC2 . #t))
  #:property prop:into-int LDAPURSW->int
  #:property prop:try-from-int int->LDAPURSW
)

(provide (struct-out LDAPURSW))

(define (int->LDAR/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 10 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LDAR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 24 30) #x8)
    (equal? (bitwise-bit-field i 26 27) 0)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply LDAR (int->LDAR/struct i))])
)

(define (LDAR->int l)
  (match-define (LDAR size l rs o0 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x8 24)
    (arithmetic-shift #x1 23)
    (arithmetic-shift l 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct LDAR (size l rs o0 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDAR->int
  #:property prop:try-from-int int->LDAR
)

(provide (struct-out LDAR))

(define int->LDARB/struct int->LDAR/struct)

(define (int->LDARB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 24 30) #x8)
    (equal? (bitwise-bit-field i 23 24) 1)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 15 16) 1)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply LDARB (int->LDARB/struct i))])
)

(define (LDARB->int l)
  (match-define (LDARB size l rs o0 rt2 rn rt) l)
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

(struct LDARB (size l rs o0 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LDARB->int
  #:property prop:try-from-int int->LDARB
)

(provide (struct-out LDARB))
