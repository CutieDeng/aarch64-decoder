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