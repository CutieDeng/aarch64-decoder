#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->SETGP/struct i)
  (list
    (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 12 16)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->SETGP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x1)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 22 24) #x3)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 14) #x0)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply SETGP (int->SETGP/struct i))])
)

(define (SETGP->int rcw)
  (match-define (SETGP sz rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift #x1 24)
    (arithmetic-shift #x3 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct SETGP (sz rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int SETGP->int
  #:property prop:try-from-int int->SETGP
)

(provide (struct-out SETGP))

(define int->SETGPN/struct int->SETGP/struct)

(define (int->SETGPN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x1)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 22 24) #x3)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 14) #x2)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply SETGPN (int->SETGPN/struct i))])
)

(define (SETGPN->int rcw)
  (match-define (SETGPN sz rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift #x1 24)
    (arithmetic-shift #x3 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct SETGPN (sz rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int SETGPN->int
  #:property prop:try-from-int int->SETGPN
)

(provide (struct-out SETGPN))

(define int->SETGPT/struct int->SETGP/struct)

(define (int->SETGPT i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x1)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 22 24) #x3)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 14) #x1)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply SETGPT (int->SETGPT/struct i))])
)

(define (SETGPT->int rcw)
  (match-define (SETGPT sz rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift #x1 24)
    (arithmetic-shift #x3 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct SETGPT (sz rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int SETGPT->int
  #:property prop:try-from-int int->SETGPT
)

(provide (struct-out SETGPT))

(define int->SETGPTN/struct int->SETGP/struct)

(define (int->SETGPTN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x1)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 22 24) #x3)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 14) #x3)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply SETGPTN (int->SETGPTN/struct i))])
)

(define (SETGPTN->int rcw)
  (match-define (SETGPTN sz rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 26)
    (arithmetic-shift #x1 24)
    (arithmetic-shift #x3 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct SETGPTN (sz rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int SETGPTN->int
  #:property prop:try-from-int int->SETGPTN
)

(provide (struct-out SETGPTN))

(define int->SETP/struct int->SETGP/struct)

(define (int->SETP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 22 24) #x3)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 14) #x0)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply SETP (int->SETP/struct i))])
)

(define (SETP->int rcw)
  (match-define (SETP sz rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift #x3 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct SETP (sz rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int SETP->int
  #:property prop:try-from-int int->SETP
)

(provide (struct-out SETP))

(define int->SETPN/struct int->SETGP/struct)

(define (int->SETPN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 22 24) #x3)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 14) #x2)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply SETPN (int->SETPN/struct i))])
)

(define (SETPN->int rcw)
  (match-define (SETPN sz rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift #x3 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct SETPN (sz rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int SETPN->int
  #:property prop:try-from-int int->SETPN
)

(provide (struct-out SETPN))

(define int->SETPT/struct int->SETGP/struct)

(define (int->SETPT i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 22 24) #x3)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 14) #x1)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply SETPT (int->SETPT/struct i))])
)

(define (SETPT->int rcw)
  (match-define (SETPT sz rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift #x3 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct SETPT (sz rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int SETPT->int
  #:property prop:try-from-int int->SETPT
)

(provide (struct-out SETPT))

(define int->SETPTN/struct int->SETGP/struct)

(define (int->SETPTN i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 27 30) #x3)
    (equal? (bitwise-bit-field i 26 27) #x0)
    (equal? (bitwise-bit-field i 24 26) #x1)
    (equal? (bitwise-bit-field i 22 24) #x3)
    (equal? (bitwise-bit-field i 21 22) #x0)
    (equal? (bitwise-bit-field i 12 14) #x3)
    (equal? (bitwise-bit-field i 10 12) #x1)
  ) #f]
  [else (apply SETPTN (int->SETPTN/struct i))])
)

(define (SETPTN->int rcw)
  (match-define (SETPTN sz rs op2 rn rd) rcw)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x3 27)
    (arithmetic-shift #x1 24)
    (arithmetic-shift #x3 22)
    (arithmetic-shift rs 16)
    (arithmetic-shift op2 12)
    (arithmetic-shift #x1 10)
    (arithmetic-shift rn 5)
    rd
  )
)

(struct SETPTN (sz rs op2 rn rd)
  #:transparent
  #:property prop:in-feature 'FEAT_MOPS
  #:property prop:into-int SETPTN->int
  #:property prop:try-from-int int->SETPTN
)

(provide (struct-out SETPTN))
