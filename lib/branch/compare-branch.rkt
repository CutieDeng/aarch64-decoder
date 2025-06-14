#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(struct CBNZ (sf imm19 rt)
  #:property prop:in-feature #hash()
  #:property prop:into-int cbnz->int
  #:property prop:try-from-int int->cbnz
)

(define (int->cbnz/struct i)
  (define sf (bitwise-bit-field i 31 32))
  (define imm19 (bitwise-bit-field i 5 24)) 
  (define rt (bitwise-bit-field i 0 5))
  (values sf imm19 rt)
)

(define CBNZ-head #x1a)

(define (int->cbnz i)
  (cond [(not (and (equal? (bitwise-bit-field i 25 31) CBNZ-head) (equal? (bitwise-bit-field i 24 25) 1))) #f]
    [else
      (define-values (sf imm19 rt) (int->cbnz/struct i))
      (CBNZ sf imm19 rt)
    ]
  )
)

(define (cbnz->int cbnz)
  (match-define (CBNZ sf imm19 rt) cbnz)
  (bitwise-ior
    (arithmetic-shift sf 31)
    (arithmetic-shift CBNZ-head 25)
    (arithmetic-shift 1 24)
    (arithmetic-shift imm19 5)
    rt
  )
)

(provide (struct-out CBNZ))

(struct CBZ (sf imm19 rt)
  #:property prop:in-feature #hash()
  #:property prop:into-int cbz->int
  #:property prop:try-from-int int->cbz
)

(define (int->cbz i)
  (cond [(not (and (equal? (bitwise-bit-field i 25 31) CBNZ-head) (equal? (bitwise-bit-field i 24 25) 0))) #f]
    [else
      (define-values (sf imm19 rt) (int->cbnz/struct i))
      (CBZ sf imm19 rt)
    ]
  )
)

(define (cbz->int cbnz)
  (match-define (CBNZ sf imm19 rt) cbnz)
  (bitwise-ior
    (arithmetic-shift sf 31)
    (arithmetic-shift CBNZ-head 25)
    (arithmetic-shift imm19 5)
    rt
  )
)

(provide (struct-out CBZ))