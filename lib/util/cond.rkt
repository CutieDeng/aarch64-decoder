#lang racket

(define (i4->cond i4) (match i4
  [#b0000 'EQ]
  [#b0001 'NE]
  [#b0010 'CS]
  [#b0011 'CC]
  [#b0100 'MI]
  [#b0101 'PL]
  [#b0110 'VS]
  [#b0111 'VC]
  [#b1000 'HI]
  [#b1001 'LS]
  [#b1010 'GE]
  [#b1011 'LT]
  [#b1100 'GT]
  [#b1101 'LE]
  [#b1110 'AL]
  [#b1111 'NV]
))

(define (cond->i4 c)
  (let find ([i 0])
    (cond
      [(symbol=? (i4->cond i) c) i]
      [else (find (add1 i))]
    ))
)

(provide cond->i4 i4->cond)
