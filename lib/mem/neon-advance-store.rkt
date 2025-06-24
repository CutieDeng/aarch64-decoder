#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->ST1/s/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 21 22)
    (bitwise-bit-field i 16 17)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 10 12)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->ST1/s i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1a)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 17 21) 0)
    (equal? (bitwise-bit-field i 16 17) 0)
    (equal? (bitwise-bit-field i 13 14) 0)
  ) #f]
  [else (apply ST1/s (int->ST1/s/struct i))])
)

(define (ST1/s->int st1)
  (match-define (ST1/s q l r o2 opcode s size rn rt) st1)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x1a 23)
    (arithmetic-shift l 22)
    (arithmetic-shift r 21)
    (arithmetic-shift o2 16)
    (arithmetic-shift opcode 13)
    (arithmetic-shift s 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST1/s (q l r o2 opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST1/s->int
  #:property prop:try-from-int int->ST1/s
)

(provide (struct-out ST1/s))

(define (int->ST1/s/Post/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 21 22)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 13 16)
    (bitwise-bit-field i 12 13)
    (bitwise-bit-field i 10 12)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->ST1/s/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1b)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 13 14) 0)
  ) #f]
  [else (apply ST1/s/Post (int->ST1/s/Post/struct i))])
)

(define (ST1/s/Post->int st1)
  (match-define (ST1/s/Post q l r rm opcode s size rn rt) st1)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x1a 23)
    (arithmetic-shift l 22)
    (arithmetic-shift r 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift opcode 13)
    (arithmetic-shift s 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST1/s/Post (q l r rm opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST1/s/Post->int
  #:property prop:try-from-int int->ST1/s/Post
)

(provide (struct-out ST1/s/Post))

(define (int->ST1/m/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 12 16)
    (bitwise-bit-field i 10 12)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->ST1/m i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x18)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 16 22) 0)
    (equal? (bitwise-bit-field i 13 14) 1)
  ) #f]
  [else (apply ST1/m (int->ST1/m/struct i))])
)

(define (ST1/m->int st1)
  (match-define (ST1/m q l opcode size rn rt) st1)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x18 23)
    (arithmetic-shift l 22)
    (arithmetic-shift opcode 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST1/m (q l opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST1/m->int
  #:property prop:try-from-int int->ST1/m
)

(provide (struct-out ST1/m))

(define (int->ST1/m/Post/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 12 16)
    (bitwise-bit-field i 10 12)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->ST1/m/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x19)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 13 14) 1)
  ) #f]
  [else (apply ST1/m/Post (int->ST1/m/Post/struct i))])
)

(define (ST1/m/Post->int st1)
  (match-define (ST1/m/Post q l rm opcode size rn rt) st1)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x19 23)
    (arithmetic-shift l 22)
    (arithmetic-shift rm 16)
    (arithmetic-shift opcode 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST1/m/Post (q l rm opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST1/m/Post->int
  #:property prop:try-from-int int->ST1/m/Post
)

(provide (struct-out ST1/m/Post))

(define int->ST2/s/struct int->ST1/s/struct)

(define (int->ST2/s i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1a)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 17 21) 0)
    (equal? (bitwise-bit-field i 16 17) 0)
    (equal? (bitwise-bit-field i 13 14) 0)
  ) #f]
  [else (apply ST2/s (int->ST2/s/struct i))])
)

(define (ST2/s->int st2)
  (match-define (ST2/s q l r o2 opcode s size rn rt) st2)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x1a 23)
    (arithmetic-shift l 22)
    (arithmetic-shift r 21)
    (arithmetic-shift o2 16)
    (arithmetic-shift opcode 13)
    (arithmetic-shift s 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST2/s (q l r o2 opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST2/s->int
  #:property prop:try-from-int int->ST2/s
)

(provide (struct-out ST2/s))

(define int->ST2/s/Post/struct int->ST1/s/Post/struct)

(define (int->ST2/s/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1b)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 13 14) 0)
  ) #f]
  [else (apply ST2/s/Post (int->ST2/s/Post/struct i))])
)

(define (ST2/s/Post->int st2)
  (match-define (ST2/s/Post q l r rm opcode s size rn rt) st2)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x1b 23)
    (arithmetic-shift l 22)
    (arithmetic-shift r 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift opcode 13)
    (arithmetic-shift s 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST2/s/Post (q l r rm opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST2/s/Post->int
  #:property prop:try-from-int int->ST2/s/Post
)

(provide (struct-out ST2/s/Post))

(define int->ST2/m/struct int->ST1/m/struct)

(define (int->ST2/m i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x18)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 16 22) 0)
    (equal? (bitwise-bit-field i 12 16) #x8)
  ) #f]
  [else (apply ST2/m (int->ST2/m/struct i))])
)

(define (ST2/m->int st2)
  (match-define (ST2/m q l opcode size rn rt) st2)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x18 23)
    (arithmetic-shift l 22)
    (arithmetic-shift opcode 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST2/m (q l opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST2/m->int
  #:property prop:try-from-int int->ST2/m
)

(provide (struct-out ST2/m))

(define int->ST2/m/Post/struct int->ST1/m/Post/struct)

(define (int->ST2/m/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x19)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 12 16) #x8)
  ) #f]
  [else (apply ST2/m/Post (int->ST2/m/Post/struct i))])
)

(define (ST2/m/Post->int st2)
  (match-define (ST2/m/Post q l rm opcode size rn rt) st2)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x19 23)
    (arithmetic-shift l 22)
    (arithmetic-shift rm 16)
    (arithmetic-shift opcode 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST2/m/Post (q l rm opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST2/m/Post->int
  #:property prop:try-from-int int->ST2/m/Post
)

(provide (struct-out ST2/m/Post))

(define int->ST3/s/struct int->ST1/s/struct)

(define (int->ST3/s i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1a)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 17 21) 0)
    (equal? (bitwise-bit-field i 16 17) 0)
    (equal? (bitwise-bit-field i 13 14) 1)
  ) #f]
  [else (apply ST3/s (int->ST3/s/struct i))])
)

(define (ST3/s->int st3)
  (match-define (ST3/s q l r o2 opcode s size rn rt) st3)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x1a 23)
    (arithmetic-shift l 22)
    (arithmetic-shift r 21)
    (arithmetic-shift o2 16)
    (arithmetic-shift opcode 13)
    (arithmetic-shift s 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST3/s (q l r o2 opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST3/s->int
  #:property prop:try-from-int int->ST3/s
)

(provide (struct-out ST3/s))

(define int->ST3/s/Post/struct int->ST1/s/Post/struct)

(define (int->ST3/s/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1b)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 13 14) 1)
  ) #f]
  [else (apply ST3/s/Post (int->ST3/s/Post/struct i))])
)

(define (ST3/s/Post->int st3)
  (match-define (ST3/s/Post q l r rm opcode s size rn rt) st3)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x1b 23)
    (arithmetic-shift l 22)
    (arithmetic-shift r 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift opcode 13)
    (arithmetic-shift s 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST3/s/Post (q l r rm opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST3/s/Post->int
  #:property prop:try-from-int int->ST3/s/Post
)

(provide (struct-out ST3/s/Post))

(define int->ST3/m/struct int->ST1/m/struct)

(define (int->ST3/m i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x18)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 16 22) 0)
    (equal? (bitwise-bit-field i 12 16) #x4)
  ) #f]
  [else (apply ST3/m (int->ST3/m/struct i))])
)

(define (ST3/m->int st3)
  (match-define (ST3/m q l opcode size rn rt) st3)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x18 23)
    (arithmetic-shift l 22)
    (arithmetic-shift opcode 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST3/m (q l opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST3/m->int
  #:property prop:try-from-int int->ST3/m
)

(provide (struct-out ST3/m))

(define int->ST3/m/Post/struct int->ST1/m/Post/struct)

(define (int->ST3/m/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x19)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 12 16) #x4)
  ) #f]
  [else (apply ST3/m/Post (int->ST3/m/Post/struct i))])
)

(define (ST3/m/Post->int st3)
  (match-define (ST3/m/Post q l rm opcode size rn rt) st3)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x19 23)
    (arithmetic-shift l 22)
    (arithmetic-shift rm 16)
    (arithmetic-shift opcode 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST3/m/Post (q l rm opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST3/m/Post->int
  #:property prop:try-from-int int->ST3/m/Post
)

(provide (struct-out ST3/m/Post))

(define int->ST4/s/struct int->ST1/s/struct)

(define (int->ST4/s i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1a)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 17 21) 0)
    (equal? (bitwise-bit-field i 16 17) 0)
    (equal? (bitwise-bit-field i 13 14) 1)
  ) #f]
  [else (apply ST4/s (int->ST4/s/struct i))])
)

(define (ST4/s->int st4)
  (match-define (ST4/s q l r o2 opcode s size rn rt) st4)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x1a 23)
    (arithmetic-shift l 22)
    (arithmetic-shift r 21)
    (arithmetic-shift o2 16)
    (arithmetic-shift opcode 13)
    (arithmetic-shift s 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST4/s (q l r o2 opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST4/s->int
  #:property prop:try-from-int int->ST4/s
)

(provide (struct-out ST4/s))

(define int->ST4/s/Post/struct int->ST1/s/Post/struct)

(define (int->ST4/s/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1b)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 13 14) 1)
  ) #f]
  [else (apply ST4/s/Post (int->ST4/s/Post/struct i))])
)

(define (ST4/s/Post->int st4)
  (match-define (ST4/s/Post q l r rm opcode s size rn rt) st4)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x1b 23)
    (arithmetic-shift l 22)
    (arithmetic-shift r 21)
    (arithmetic-shift rm 16)
    (arithmetic-shift opcode 13)
    (arithmetic-shift s 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST4/s/Post (q l r rm opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST4/s/Post->int
  #:property prop:try-from-int int->ST4/s/Post
)

(provide (struct-out ST4/s/Post))

(define int->ST4/m/struct int->ST1/m/struct)

(define (int->ST4/m i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x18)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 16 22) 0)
    (equal? (bitwise-bit-field i 12 16) 0)
  ) #f]
  [else (apply ST4/m (int->ST4/m/struct i))])
)

(define (ST4/m->int st4)
  (match-define (ST4/m q l opcode size rn rt) st4)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x18 23)
    (arithmetic-shift l 22)
    (arithmetic-shift opcode 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST4/m (q l opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST4/m->int
  #:property prop:try-from-int int->ST4/m
)

(provide (struct-out ST4/m))

(define int->ST4/m/Post/struct int->ST1/m/Post/struct)

(define (int->ST4/m/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x19)
    (equal? (bitwise-bit-field i 22 23) 0)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 12 16) 0)
  ) #f]
  [else (apply ST4/m/Post (int->ST4/m/Post/struct i))])
)

(define (ST4/m/Post->int st4)
  (match-define (ST4/m/Post q l rm opcode size rn rt) st4)
  (bitwise-ior
    (arithmetic-shift q 30)
    (arithmetic-shift #x19 23)
    (arithmetic-shift l 22)
    (arithmetic-shift rm 16)
    (arithmetic-shift opcode 12)
    (arithmetic-shift size 10)
    (arithmetic-shift rn 5)
    rt
  )
)

(struct ST4/m/Post (q l rm opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int ST4/m/Post->int
  #:property prop:try-from-int int->ST4/m/Post
)

(provide (struct-out ST4/m/Post))
