#lang racket

(define-values (prop:in-feature in-feature? in-feature-ref)
  (make-struct-type-property 'in-feature))

(provide prop:in-feature in-feature? in-feature-ref)
