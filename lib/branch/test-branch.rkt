#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(struct TBNZ (b5 b40 imm14 rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int tbnz->int
  #:property prop:try-from-int int->tbnz
)

(define (int->tbnz/struct i)
  (define b5 (bitwise-bit-field i 31 32))
  (define b40 (bitwise-bit-field i 19 24)) 
  (define imm14 (bitwise-bit-field i 5 19))
  (define rt (bitwise-bit-field i 0 5))
  (list b5 b40 imm14 rt)
)

(define TBNZ-head #x1b)

(define (int->tbnz i)
  (cond [(not (and (equal? (bitwise-bit-field i 25 31) TBNZ-head) (equal? (bitwise-bit-field i 24 25) 1))) #f]
    [else
      (apply TBNZ (int->tbnz/struct i))
    ]
  )
)

(define (tbnz->int tbnz)
  (match-define (TBNZ b5 b40 imm14 rt) tbnz)
  (bitwise-ior
    (arithmetic-shift b5 31)
    (arithmetic-shift TBNZ-head 25)
    (arithmetic-shift 1 24)
    (arithmetic-shift b40 19)
    (arithmetic-shift imm14 5)
    rt
  )
)

(provide (struct-out TBNZ))

(struct TBZ (b5 b40 imm14 rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int tbz->int
  #:property prop:try-from-int int->tbz
)

(define (int->tbz i)
  (cond [(not (and (equal? (bitwise-bit-field i 25 31) TBNZ-head) (equal? (bitwise-bit-field i 24 25) 0))) #f]
    [else
      (apply TBZ (int->tbnz/struct i))
    ]
  )
)

(define (tbz->int tbz)
  (match-define (TBZ b5 b40 imm14 rt) tbz)
  (bitwise-ior
    (arithmetic-shift b5 31)
    (arithmetic-shift TBNZ-head 25)
    (arithmetic-shift b40 19)
    (arithmetic-shift imm14 5)
    rt
  )
)

(provide (struct-out TBZ))