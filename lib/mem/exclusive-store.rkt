#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->STXR/struct i)
  (list (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 10 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STXR i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 24 30) #x8)
    (equal? (bitwise-bit-field i 23 24) 0)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply STXR (int->STXR/struct i))])
)

(define (STXR->int l)
  (match-define (STXR size l rs o0 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x8 24)
    (arithmetic-shift l 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STXR (size l rs o0 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STXR->int
  #:property prop:try-from-int int->STXR
)

(provide (struct-out STXR))

(define int->STXRB/struct int->STXR/struct)

(define (int->STXRB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 0)
    (equal? (bitwise-bit-field i 24 30) #x8)
    (equal? (bitwise-bit-field i 23 24) 0)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply STXRB (int->STXRB/struct i))])
)

(define (STXRB->int l)
  (match-define (STXRB size l rs o0 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x8 24)
    (arithmetic-shift l 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STXRB (size l rs o0 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STXRB->int
  #:property prop:try-from-int int->STXRB
)

(provide (struct-out STXRB))

(define int->STXRH/struct int->STXR/struct)

(define (int->STXRH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) 1)
    (equal? (bitwise-bit-field i 24 30) #x8)
    (equal? (bitwise-bit-field i 23 24) 0)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 15 16) 0)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply STXRH (int->STXRH/struct i))])
)

(define (STXRH->int l)
  (match-define (STXRH size l rs o0 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x8 24)
    (arithmetic-shift l 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STXRH (size l rs o0 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STXRH->int
  #:property prop:try-from-int int->STXRH
)

(provide (struct-out STXRH))

(define (int->STXP/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 10 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->STXP i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) 1)
    (equal? (bitwise-bit-field i 24 30) #x8)
    (equal? (bitwise-bit-field i 23 24) 0)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 15 16) 0)
  ) #f]
  [else (apply STXP (int->STXP/struct i))])
)

(define (STXP->int l)
  (match-define (STXP sz l rs o0 rt2 rn rt) l)
  (bitwise-ior
    (arithmetic-shift 1 31)
    (arithmetic-shift sz 30)
    (arithmetic-shift #x8 24)
    (arithmetic-shift l 22)
    (arithmetic-shift 1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct STXP (sz l rs o0 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int STXP->int
  #:property prop:try-from-int int->STXP
)

(provide (struct-out STXP))
