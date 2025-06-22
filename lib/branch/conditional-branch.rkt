#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")
(require "../util/cond.rkt")

(define B.cond.header #x54)

(define (B.cond->int b)
  (match-define (B.cond imm19 c) b)
  (bitwise-ior
    (arithmetic-shift B.cond.header 24)
    (arithmetic-shift imm19 5)
    c
  )
)

(define (int->B.cond i)
  (define head (bitwise-bit-field i 24 32))
  (cond [(not (equal? head B.cond.header)) #f]
  [else
    (define subv (bitwise-bit-field i 4 5))
    (cond [(not (equal? subv 0)) #f]
    [else
      (apply B.cond (int->B.cond/struct i))
    ])
  ])
)

(define (int->B.cond/struct i)
  (define imm19 (bitwise-bit-field i 5 24))
  (define c (bitwise-bit-field i 0 4))
  (list imm19 c)
)

(struct B.cond (imm19 cond)
  #:transparent
  #:property prop:into-int B.cond->int
  #:property prop:try-from-int int->B.cond
  #:property prop:in-feature #hash())

(provide (struct-out B.cond))

(struct BC.cond (imm19 cond)
  #:transparent
  #:property prop:into-int BC.cond->int
  #:property prop:try-from-int int->BC.cond
  #:property prop:in-feature #hash((FEAT_HBC . #t)))

(define (BC.cond->int b)
  (bitwise-ior
    (arithmetic-shift B.cond.header 24)
    (arithmetic-shift (B.cond-imm19 b) 5)
    (arithmetic-shift 1 4)
    (cond->i4 (B.cond-cond b))
  )
)

(define (int->BC.cond i)
  (define head (bitwise-bit-field i 24 32))
  (cond [(not (equal? head B.cond.header)) #f]
  [else
    (define subv (bitwise-bit-field i 4 5))
    (cond [(not (equal? subv 1)) #f]
    [else
      (apply BC.cond (int->B.cond/struct i))
    ])
  ])
)

(provide (struct-out BC.cond))
