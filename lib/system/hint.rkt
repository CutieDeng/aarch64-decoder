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

(define WFE-head NOP-head)

(define WFE-literal (bitwise-ior
  (arithmetic-shift WFE-head 22)
  (arithmetic-shift #x3 16)
  (arithmetic-shift #x2 12)
  (arithmetic-shift #x2 5)
  #x1f
))

(define (int->WFE i)
  (cond [(nand (equal? i WFE-literal)) #f]
    [else (WFE)])
)

(define (WFE->int _nop)
  WFE-literal)

(struct WFE ()
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int WFE->int
  #:property prop:try-from-int int->WFE
)

(provide (struct-out WFE))

(define WFI-head NOP-head)

(define WFI-literal (bitwise-ior
  (arithmetic-shift WFI-head 22)
  (arithmetic-shift #x3 16)
  (arithmetic-shift #x2 12)
  (arithmetic-shift #x3 5)
  #x1f
))

(define (int->WFI i)
  (cond [(nand (equal? i WFI-literal)) #f]
    [else (WFI)])
)

(define (WFI->int _w)
  WFI-literal
)

(struct WFI ()
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int WFI->int
  #:property prop:try-from-int int->WFI
)

(provide (struct-out WFI))