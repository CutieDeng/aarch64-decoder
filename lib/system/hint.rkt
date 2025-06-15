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

(define SEV-head NOP-head)

(define SEV-literal (bitwise-ior
  (arithmetic-shift SEV-head 22)
  (arithmetic-shift #x3 16)
  (arithmetic-shift #x2 12)
  (arithmetic-shift #x4 5)
  #x1f
))

(define (int->SEV i)
  (cond [(nand (equal? i SEV-literal)) #f]
    [else (SEV)])
)

(define (SEV->int _w)
  SEV-literal
)

(struct SEV ()
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int SEV->int
  #:property prop:try-from-int int->SEV
)

(provide (struct-out SEV))

(define SEVL-head NOP-head)

(define SEVL-literal (bitwise-ior
  (arithmetic-shift SEVL-head 22)
  (arithmetic-shift #x3 16)
  (arithmetic-shift #x2 12)
  (arithmetic-shift #x5 5)
  #x1f
))

(define (int->SEVL i)
  (cond [(nand (equal? i SEVL-literal)) #f]
    [else (SEVL)])
)

(define (SEVL->int _w)
  SEVL-literal
)

(struct SEVL ()
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int SEVL->int
  #:property prop:try-from-int int->SEVL
)

(provide (struct-out SEVL))