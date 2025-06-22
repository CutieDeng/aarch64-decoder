#lang racket

(define-values (prop:into-int into-int? into-int-ref)
  (make-struct-type-property 'into-int))

(provide prop:into-int into-int? into-int-ref)
