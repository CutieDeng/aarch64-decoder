#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define CSDB-head #xd5032)

(define CSDB-literal (bitwise-ior
  (arithmetic-shift CSDB-head 12)
  (arithmetic-shift #x2 8)
  (arithmetic-shift #x4 5)
  #x1f
))

(define (int->CSDB i)
  (cond [(nand (equal? i CSDB-literal)) #f]
    [else (CSDB)])
)

(define (CSDB->int _cs)
  CSDB-literal
)

(struct CSDB ()
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int CSDB->int
  #:property prop:try-from-int int->CSDB
)

(provide (struct-out CSDB))

(define ESB-head CSDB-head)

(define ESB-literal (bitwise-ior
  (arithmetic-shift ESB-head 12)
  (arithmetic-shift #x2 8)
  #x1f
))

(define (int->ESB i)
  (cond [(nand (equal? i ESB-literal)) #f]
    [else (ESB)])
)

(define (ESB->int _cs)
  ESB-literal
)

(struct ESB ()
  #:transparent
  #:property prop:in-feature #hash((FEAT_RAS . #t))
  #:property prop:into-int ESB->int
  #:property prop:try-from-int int->ESB
)

(provide (struct-out ESB))

(define PSB-head CSDB-head)

(define PSB-literal (bitwise-ior
  (arithmetic-shift PSB-head 12)
  (arithmetic-shift #x2 8)
  (arithmetic-shift #x1 5)
  #x1f
))

(define (int->PSB i)
  (cond [(nand (equal? i PSB-literal)) #f]
    [else (PSB)])
)

(define (PSB->int _cs)
  PSB-literal
)

(struct PSB ()
  #:transparent
  #:property prop:in-feature #hash((FEAT_SPE . #t))
  #:property prop:into-int PSB->int
  #:property prop:try-from-int int->PSB
)

(provide (struct-out PSB))

(define SB-head #xd5033)

(define SB-literal (bitwise-ior
  (arithmetic-shift SB-head 12)
  (arithmetic-shift #x1 7)
  (arithmetic-shift #x3 5)
  #x1f
))

(define (int->SB i)
  (cond [(nand (equal? i SB-literal)) #f]
    [else (SB)])
)

(define (SB->int _cs)
  SB-literal
)

(struct SB ()
  #:transparent
  #:property prop:in-feature #hash((FEAT_SB . #t))
  #:property prop:into-int SB->int
  #:property prop:try-from-int int->SB
)

(provide (struct-out SB))

(define TSB-head CSDB-head)

(define TSB-literal (bitwise-ior
  (arithmetic-shift TSB-head 12)
  (arithmetic-shift #x2 8)
  (arithmetic-shift #x2 5)
  #x1f
))

(define (int->TSB i)
  (cond [(nand (equal? i TSB-literal)) #f]
    [else (TSB)])
)

(define (TSB->int _cs)
  TSB-literal
)

(struct TSB ()
  #:transparent
  #:property prop:in-feature #hash((FEAT_TRF . #t))
  #:property prop:into-int TSB->int
  #:property prop:try-from-int int->TSB
)

(provide (struct-out TSB))