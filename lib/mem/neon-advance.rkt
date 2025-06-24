#lang racket

(require "../util/in-feature.rkt")
(require "../util/into-int.rkt")
(require "../util/try-from-int.rkt")

(define (int->LD1/s/struct i)
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

(define (int->LD1/s i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1a)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 17 21) 0)
    (equal? (bitwise-bit-field i 16 17) 0)
    (equal? (bitwise-bit-field i 13 14) 0)
  ) #f]
  [else (apply LD1/s (int->LD1/s/struct i))])
)

(define (LD1/s->int ld1)
  (match-define (LD1/s q l r o2 opcode s size rn rt) ld1)
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

(struct LD1/s (q l r o2 opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD1/s->int
  #:property prop:try-from-int int->LD1/s
)

(provide (struct-out LD1/s))

(define (int->LD1/s/Post/struct i)
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

(define (int->LD1/s/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1b)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 13 14) 0)
  ) #f]
  [else (apply LD1/s/Post (int->LD1/s/Post/struct i))])
)

(define (LD1/s/Post->int ld1)
  (match-define (LD1/s/Post q l r rm opcode s size rn rt) ld1)
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

(struct LD1/s/Post (q l r rm opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD1/s/Post->int
  #:property prop:try-from-int int->LD1/s/Post
)

(provide (struct-out LD1/s/Post))

(define (int->LD1/m/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 12 16)
    (bitwise-bit-field i 10 12)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LD1/m i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x18)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 16 22) 0)
    (equal? (bitwise-bit-field i 13 14) 1)
  ) #f]
  [else (apply LD1/m (int->LD1/m/struct i))])
)

(define (LD1/m->int ld1)
  (match-define (LD1/m q l opcode size rn rt) ld1)
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

(struct LD1/m (q l opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD1/m->int
  #:property prop:try-from-int int->LD1/m
)

(provide (struct-out LD1/m))

(define (int->LD1/m/Post/struct i)
  (list (bitwise-bit-field i 30 31)
    (bitwise-bit-field i 22 23)
    (bitwise-bit-field i 16 21)
    (bitwise-bit-field i 12 16)
    (bitwise-bit-field i 10 12)
    (bitwise-bit-field i 5 10)
    (bitwise-bit-field i 0 5))
)

(define (int->LD1/m/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x19)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 13 14) 1)
  ) #f]
  [else (apply LD1/m/Post (int->LD1/m/Post/struct i))])
)

(define (LD1/m/Post->int ld1)
  (match-define (LD1/m/Post q l rm opcode size rn rt) ld1)
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

(struct LD1/m/Post (q l rm opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD1/m/Post->int
  #:property prop:try-from-int int->LD1/m/Post
)

(provide (struct-out LD1/m/Post))

(define int->LD2/s/struct int->LD1/s/struct)

(define (int->LD2/s i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1a)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 17 21) 0)
    (equal? (bitwise-bit-field i 16 17) 0)
    (equal? (bitwise-bit-field i 13 14) 0)
  ) #f]
  [else (apply LD2/s (int->LD2/s/struct i))])
)

(define (LD2/s->int ld2)
  (match-define (LD2/s q l r o2 opcode s size rn rt) ld2)
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

(struct LD2/s (q l r o2 opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD2/s->int
  #:property prop:try-from-int int->LD2/s
)

(provide (struct-out LD2/s))

(define int->LD2/s/Post/struct int->LD1/s/Post/struct)

(define (int->LD2/s/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1b)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 13 14) 0)
  ) #f]
  [else (apply LD2/s/Post (int->LD2/s/Post/struct i))])
)

(define (LD2/s/Post->int ld2)
  (match-define (LD2/s/Post q l r rm opcode s size rn rt) ld2)
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

(struct LD2/s/Post (q l r rm opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD2/s/Post->int
  #:property prop:try-from-int int->LD2/s/Post
)

(provide (struct-out LD2/s/Post))

(define int->LD2/m/struct int->LD1/m/struct)

(define (int->LD2/m i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x18)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 16 22) 0)
    (equal? (bitwise-bit-field i 12 16) #x8)
  ) #f]
  [else (apply LD2/m (int->LD2/m/struct i))])
)

(define (LD2/m->int ld2)
  (match-define (LD2/m q l opcode size rn rt) ld2)
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

(struct LD2/m (q l opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD2/m->int
  #:property prop:try-from-int int->LD2/m
)

(provide (struct-out LD2/m))

(define int->LD2/m/Post/struct int->LD1/m/Post/struct)

(define (int->LD2/m/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x19)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 12 16) #x8)
  ) #f]
  [else (apply LD2/m/Post (int->LD2/m/Post/struct i))])
)

(define (LD2/m/Post->int ld2)
  (match-define (LD2/m/Post q l rm opcode size rn rt) ld2)
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

(struct LD2/m/Post (q l rm opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD2/m/Post->int
  #:property prop:try-from-int int->LD2/m/Post
)

(provide (struct-out LD2/m/Post))

(define int->LD3/s/struct int->LD1/s/struct)

(define (int->LD3/s i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1a)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 1)
    (equal? (bitwise-bit-field i 17 21) 0)
    (equal? (bitwise-bit-field i 16 17) 0)
    (equal? (bitwise-bit-field i 13 14) 1)
  ) #f]
  [else (apply LD3/s (int->LD3/s/struct i))])
)

(define (LD3/s->int ld3)
  (match-define (LD3/s q l r o2 opcode s size rn rt) ld3)
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

(struct LD3/s (q l r o2 opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD3/s->int
  #:property prop:try-from-int int->LD3/s
)

(provide (struct-out LD3/s))

(define int->LD3/s/Post/struct int->LD1/s/Post/struct)

(define (int->LD3/s/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x1b)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 13 14) 0)
  ) #f]
  [else (apply LD3/s/Post (int->LD3/s/Post/struct i))])
)

(define (LD3/s/Post->int ld3)
  (match-define (LD3/s/Post q l r rm opcode s size rn rt) ld3)
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

(struct LD3/s/Post (q l r rm opcode s size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD3/s/Post->int
  #:property prop:try-from-int int->LD3/s/Post
)

(provide (struct-out LD3/s/Post))

(define int->LD3/m/struct int->LD1/m/struct)

(define (int->LD3/m i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x18)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 16 22) 0)
    (equal? (bitwise-bit-field i 12 16) #x4)
  ) #f]
  [else (apply LD3/m (int->LD3/m/struct i))])
)

(define (LD3/m->int ld3)
  (match-define (LD3/m q l opcode size rn rt) ld3)
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

(struct LD3/m (q l opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD3/m->int
  #:property prop:try-from-int int->LD3/m
)

(provide (struct-out LD3/m))

(define int->LD3/m/Post/struct int->LD1/m/Post/struct)

(define (int->LD3/m/Post i)
  (cond [(nand
    (equal? (bitwise-bit-field i 31 32) 0)
    (equal? (bitwise-bit-field i 23 30) #x19)
    (equal? (bitwise-bit-field i 22 23) 1)
    (equal? (bitwise-bit-field i 21 22) 0)
    (equal? (bitwise-bit-field i 12 16) #x4)
  ) #f]
  [else (apply LD3/m/Post (int->LD3/m/Post/struct i))])
)

(define (LD3/m/Post->int ld3)
  (match-define (LD3/m/Post q l rm opcode size rn rt) ld3)
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

(struct LD3/m/Post (q l rm opcode size rn rt)
  #:transparent
  #:property prop:in-feature #hash()
  #:property prop:into-int LD3/m/Post->int
  #:property prop:try-from-int int->LD3/m/Post
)

(provide (struct-out LD3/m/Post))
