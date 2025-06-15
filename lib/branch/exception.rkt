#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define ERET-literal (bitwise-ior
  (arithmetic-shift #x6b 25)    
  (arithmetic-shift #x4 21)
  (arithmetic-shift #x1f 16)
  (arithmetic-shift #x1f 5)
))

(define (int->ERET i)
  (cond [(nand (equal? i ERET-literal)) #f]
    [else (ERET)])
)

(define (ERET->int _e)
  ERET-literal
)

(struct ERET ()
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ERET->int
  #:property prop:try-from-int int->ERET
)

(provide (struct-out ERET))