#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->CAS/struct i)
  (list
    (bitwise-bit-field i 30 32)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->CAS i)
  (cond [(nand (equal? (bitwise-bit-field i 31 32) #x1)
    (equal? (bitwise-bit-field i 23 30) #x11)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply CAS (int->CAS/struct i))])
)

(define (CAS->int swp)
  (match-define (CAS size l rs o0 rn rt) swp)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x11 23)
    (arithmetic-shift l 22)
    (arithmetic-shift 1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift #x1f 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct CAS (size l rs o0 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int CAS->int
  #:property prop:try-from-int int->CAS
)

(provide (struct-out CAS))

(define int->CASB/struct int->CAS/struct)

(define (int->CASB i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) #x0)
    (equal? (bitwise-bit-field i 23 30) #x11)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply CASB (int->CASB/struct i))])
)

(define (CASB->int swp)
  (match-define (CASB size l rs o0 rn rt) swp)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x11 23)
    (arithmetic-shift l 22)
    (arithmetic-shift 1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift #x1f 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct CASB (size l rs o0 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int CASB->int
  #:property prop:try-from-int int->CASB
)

(provide (struct-out CASB))

(define int->CASH/struct int->CAS/struct)

(define (int->CASH i)
  (cond [(nand (equal? (bitwise-bit-field i 30 32) #x1)
    (equal? (bitwise-bit-field i 23 30) #x11)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply CASH (int->CASH/struct i))])
)

(define (CASH->int swp)
  (match-define (CASH size l rs o0 rn rt) swp)
  (bitwise-ior
    (arithmetic-shift size 30)
    (arithmetic-shift #x11 23)
    (arithmetic-shift l 22)
    (arithmetic-shift 1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift #x1f 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct CASH (size l rs o0 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int CASH->int
  #:property prop:try-from-int int->CASH
)

(provide (struct-out CASH))

(define (int->CASP/struct i)
  (list
    (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 15 16)
    (bitwise-bit-field i 10 15)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->CASP i)
  (cond [(nand 
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 24 30) #x8)
    (equal? (bitwise-bit-field i 23 24) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 10 15) #x1f)
  ) #f]
  [else (apply CASP (int->CASP/struct i))])
)

(define (CASP->int swp)
  (match-define (CASP sz l rs o0 rt2 rn rt) swp)
  (bitwise-ior
    (arithmetic-shift sz 30)
    (arithmetic-shift #x8 24)
    (arithmetic-shift l 22)
    (arithmetic-shift 1 21)
    (arithmetic-shift rs 16)
    (arithmetic-shift o0 15)
    (arithmetic-shift rt2 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct CASP (sz l rs o0 rt2 rn rt)
  #:transparent
  #:property prop:in-feature #hash((FEAT_LSE . #t))
  #:property prop:into-int CASP->int
  #:property prop:try-from-int int->CASP
)

(provide (struct-out CASP))
