#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->RCWCAS/struct i)
  (list
    (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 23 24)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->RCWCAS i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) #x0)
    (equal? (bitwise-bit-field i 30 31) #x0)
    (equal? (bitwise-bit-field i 24 30) #x19)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 16) #x2)
  ) #f]
  [else (apply RCWCAS (int->RCWCAS/struct i))])
)

(define (RCWCAS->int swp)
  (match-define (RCWCAS s a r rs rn rt) swp)
  (bitwise-ior
    (arithmetic-shift s 30)
    (arithmetic-shift #x19 24)
    (arithmetic-shift a 23)
    (arithmetic-shift r 22)
    (arithmetic-shift #x1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift #x2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct RCWCAS (s a r rs rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_THE . #t))
  #:property prop:into-int RCWCAS->int
  #:property prop:try-from-int int->RCWCAS
)

(provide (struct-out RCWCAS))
