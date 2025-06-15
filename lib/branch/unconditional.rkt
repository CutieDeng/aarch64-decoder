#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(struct B (imm26)
  #:property prop:in-feature #hash()
  #:property prop:into-int B->int
  #:property prop:try-from-int int->B
)

(define B-head #x01)

(define (int->B/struct i)
  (list (bitwise-bit-field i 0 26))
)


(define (int->B i)
  (cond [(not (and (equal? (bitwise-bit-field i 26 31) B-head) (equal? (bitwise-bit-field i 31 32) 0))) #f]
    [else
      (apply B (int->B/struct i))
    ]
  )
)

(define (B->int b)
  (match-define (B imm26) b)
  (bitwise-ior
    (arithmetic-shift B-head 26)
    imm26
  )
)

(provide (struct-out B))

(struct BL (imm26)
  #:property prop:in-feature #hash()
  #:property prop:into-int BL->int
  #:property prop:try-from-int int->BL
)

(define (int->BL i)
  (cond [(not (and (equal? (bitwise-bit-field i 26 31) B-head) (equal? (bitwise-bit-field i 31 32) 1))) #f]
    [else
      (apply BL (int->B/struct i))
    ]
  )
)

(define (BL->int b)
  (match-define (BL imm26) b)
  (bitwise-ior
    (arithmetic-shift 1 31)
    (arithmetic-shift B-head 26)
    imm26
  )
)

(provide (struct-out BL))