#lang racket

(define-values (prop:try-from-int try-from-int? try-from-int-ref)
  (make-struct-type-property 'try-from-int))

(provide prop:try-from-int try-from-int? try-from-int-ref)