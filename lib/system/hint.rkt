#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define NOP-head #x354)

(define NOP-literal (bitwise-ior
  (arithmetic-shift NOP-head 22)
  (arithmetic-shift #x3 16)
  (arithmetic-shift #x2 12)
  #x1f
))

(define (int->NOP i)
  (cond [(nand (equal? i NOP-literal)) #f]
    [else (NOP)])
)

(define (NOP->int _nop)
  NOP-literal)

(struct NOP ()
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int NOP->int
  #:property prop:try-from-int int->NOP
)

(provide (struct-out NOP))

(define YIELD-head NOP-head)

(define YIELD-literal (bitwise-ior
  (arithmetic-shift YIELD-head 22)
  (arithmetic-shift #x3 16)
  (arithmetic-shift #x2 12)
  (arithmetic-shift #x1 5)
  #x1f
))

(define (int->YIELD i)
  (cond [(nand (equal? i YIELD-literal)) #f]
    [else (YIELD)])
)

(define (YIELD->int _nop)
  YIELD-literal)

(struct YIELD ()
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int YIELD->int
  #:property prop:try-from-int int->YIELD
)

(provide (struct-out YIELD))