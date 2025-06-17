#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

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

(struct B (imm26)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int B->int
  #:property prop:try-from-int int->B
)

(provide (struct-out B))

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

(struct BL (imm26)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int BL->int
  #:property prop:try-from-int int->BL
)

(provide (struct-out BL))

(define (int->BLR i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) BLR-head)
    (equal? (bitwise-bit-field i 24 25) 0)
    (equal? (bitwise-bit-field i 23 24) 0)
    (equal? (bitwise-bit-field i 21 23) 1)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 12 16) 0)
    (equal? (bitwise-bit-field i 11 12) 0)
    (equal? (bitwise-bit-field i 10 11) 0)
    (equal? (bitwise-bit-field i 0 5) 0)
    ) #f]
    [else (apply BLR (int->BLR/struct i))]
  )
)

(define BLR-head #x6b)

(define (int->BLR/struct i)
  (define rn (bitwise-bit-field i 5 10))
  (list rn)
)

(define (BLR->int blr)
  (match-define (BLR rn) blr)
  (bitwise-ior
    (arithmetic-shift BLR-head 25)
    (arithmetic-shift 1 21)
    (arithmetic-shift #x1f 16)
    (arithmetic-shift rn 5)
  )
)

(struct BLR (rn)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int BLR->int
  #:property prop:try-from-int int->BLR
)

(provide (struct-out BLR))

(define int->BR/struct int->BLR/struct)

(define BR-head BLR-head)

(define (int->BR i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) BR-head)
    (equal? (bitwise-bit-field i 24 25) 0)
    (equal? (bitwise-bit-field i 23 24) 0)
    (equal? (bitwise-bit-field i 21 23) 0)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 12 16) 0)
    (equal? (bitwise-bit-field i 11 12) 0)
    (equal? (bitwise-bit-field i 10 11) 0)
    (equal? (bitwise-bit-field i 0 5) 0)
    ) #f]
    [else (apply BR (int->BR/struct i))])
)

(define (BR->int br)
  (match-define (BR rn) br)
  (bitwise-ior
    (arithmetic-shift BR-head 25)
    (arithmetic-shift #x1f 16)
    (arithmetic-shift rn 5)
  )
)

(struct BR (rn)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int BR->int
  #:property prop:try-from-int int->BR
)

(provide (struct-out BR))

(define int->RET/struct int->BR/struct)
(define RET-head BR-head)

(define (int->RET i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) RET-head)
    (equal? (bitwise-bit-field i 24 25) 0)
    (equal? (bitwise-bit-field i 23 24) 0)
    (equal? (bitwise-bit-field i 21 23) #x2)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 12 16) 0)
    (equal? (bitwise-bit-field i 11 12) 0)
    (equal? (bitwise-bit-field i 10 11) 0)
    (equal? (bitwise-bit-field i 0 5) 0)
    ) #f]
    [else (apply RET (int->RET/struct i))])
)

(define (RET->int r)
  (match-define (RET rn) r)
  (bitwise-ior
    (arithmetic-shift RET-head 25)
    (arithmetic-shift #x2 21)
    (arithmetic-shift #x1f 16)
    (arithmetic-shift rn 5)
  )
)

(struct RET (rn)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int RET->int
  #:property prop:try-from-int int->RET
)

(provide (struct-out RET))

(define (int->BLRAA/struct i)
  (define z (bitwise-bit-field i 24 25))
  (define m (bitwise-bit-field i 10 11))
  (define rn (bitwise-bit-field i 5 10))
  (define rm (bitwise-bit-field i 0 5))
  (list z m rn rm)
)

(define BLRAA-head BLR-head)

(define (int->BLRAA i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 25 32) BLRAA-head)
    (equal? (bitwise-bit-field i 23 24) 0)
    (equal? (bitwise-bit-field i 21 23) 1)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 12 16) 0)
    (equal? (bitwise-bit-field i 11 12) 1)
    ) #f]
    [else (apply BLRAA (int->BLRAA/struct i))])
)

(define (BLRAA->int i)
  (match-define (BLRAA z m rn rm) i)
  (bitwise-ior
    (arithmetic-shift BLRAA-head 25)
    (arithmetic-shift z 24)
    (arithmetic-shift #x1 21)
    (arithmetic-shift #x1f 16)
    (arithmetic-shift 1 11)
    (arithmetic-shift m 10)
    (arithmetic-shift rn 5)
    rm
  )
)

(struct BLRAA (z m rn rm)
  #:transparent
  #:property prop:in-feature #hash((FEAT_Pauth . #t))
  #:property prop:into-int BLRAA->int
  #:property prop:try-from-int int->BLRAA
)

(provide (struct-out BLRAA))

(define int->BRAA/struct int->BLRAA/struct)

(define BRAA-head BLR-head)

(define (int->BRAA i)
  (cond [(nand
    (equal? (bitwise-bit-field i 25 32) BRAA-head)
    (equal? (bitwise-bit-field i 23 24) 0)
    (equal? (bitwise-bit-field i 21 23) 0)
    (equal? (bitwise-bit-field i 16 21) #x1f)
    (equal? (bitwise-bit-field i 12 16) 0)
    (equal? (bitwise-bit-field i 11 12) 1)
    ) #f]
    [else (apply BRAA (int->BRAA/struct i))])
)

(define (BRAA->int braa)
  (match-define (BRAA z m rn rm) braa)
  (bitwise-ior
    (arithmetic-shift BRAA-head 25)
    (arithmetic-shift z 24)
    (arithmetic-shift #x1f 16)
    (arithmetic-shift 1 11)
    (arithmetic-shift m 10)
    (arithmetic-shift rn 5)
    rm
  )
)

(struct BRAA (z m rn rm)
  #:transparent
  #:property prop:in-feature #hash((FEAT_Pauth . #t))
  #:property prop:into-int BRAA->int
  #:property prop:try-from-int int->BRAA
)

(provide (struct-out BRAA))