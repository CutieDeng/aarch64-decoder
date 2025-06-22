#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define PACIA-head #xd6)

(define PACIA->int (error 'PACIA "unimpl"))
(define int->PACIA (error 'PACIA "unimpl"))

(struct PACIA (sf S opcode2 z rn rd)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int PACIA->int
  #:property prop:try-from-int int->PACIA
)

(provide (struct-out PACIA))
