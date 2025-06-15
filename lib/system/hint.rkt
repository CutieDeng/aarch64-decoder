#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define NOP-literal (bitwise-ior
  (arithmetic-shift #x354 22)
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