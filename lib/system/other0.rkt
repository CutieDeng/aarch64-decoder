#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define WFET-head #xd5031)

(define (int->WFET/struct i)
  (list (bitwise-bit-field i 0 5)
    )
)

(define (int->WFET i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 12 32) WFET-head)
    (equal? (bitwise-bit-field i 8 12) #x0)
    (equal? (bitwise-bit-field i 5 8) #x0)
    ) #f]
    [else (apply WFET (int->WFET/struct i))])
)

(define (WFET->int e)
  (match-define (WFET rd) e)
  (bitwise-ior
    (arithmetic-shift WFET-head 12)
    rd
  )
)

(struct WFET (rd)
  #:transparent
  #:property prop:in-feature #hash((FEAT_WFxT . #t))
  #:property prop:into-int WFET->int
  #:property prop:try-from-int int->WFET
)

(provide (struct-out WFET))