#lang racket/base

(require racket/math)
(require racket/contract/base)
(require racket/contract/region)

; Operations
(struct/contract un-op ([a natural?]) #:transparent)
(struct/contract bin-op ([a natural?] [b natural?]) #:transparent)
(struct/contract op-lut ([idx natural?] [lut (listof natural?)]) #:transparent)
(define (op? op)
  (or (un-op? op) (bin-op? op) (op-lut? op)))

(struct op-id un-op ())
(struct op-const un-op ())
(struct op-add bin-op ())
(struct op-mul bin-op ())
(struct op-and bin-op ())
(struct op-or bin-op ())
(struct op-xor bin-op ())
(struct op-sll bin-op ())
(struct op-srl bin-op ())
(struct op-lt bin-op ())
(struct op-gt bin-op ())

(define (bool-op? op)
  (or (op-lt? op) (op-gt? op)))

(provide (struct-out un-op)
         (struct-out bin-op)
         (struct-out op-lut)
         op?
         (struct-out op-id)
         (struct-out op-const)
         (struct-out op-add)
         (struct-out op-mul)
         (struct-out op-and)
         (struct-out op-or)
         (struct-out op-xor)
         (struct-out op-sll)
         (struct-out op-srl)
         (struct-out op-lt)
         (struct-out op-gt)
         bool-op?)

; Statements
(struct stmt ())
(define block? (listof stmt?))
(struct/contract stmt-assign stmt ([lhs natural?] [op op?]) #:transparent)
(struct/contract stmt-while
                 stmt
                 ((cond
                    natural?)
                  [body block?])
                 #:transparent)
(struct/contract stmt-if
                 stmt
                 ((cond
                    natural?)
                  [body block?])
                 #:transparent)

(provide (struct-out stmt)
         block?
         (struct-out stmt-assign)
         (struct-out stmt-while)
         (struct-out stmt-if))

; Functions are stored in a basic form, as a list of statements which are
; either direct assignments from operations acting on variables (by index),
; or while loops themselves containing a list of statements for the body.
;
; Each variable has an index, and width (stored in widths at the associated
; index). The return value is stored at index 0 and the arguments in 1 through
; arity, with the remaining indices being used for arbitrary other variables.
; There should be no gaps between indices in widths.
;
; ast contains the actual function contents as a list of statements
(struct/contract ast-function
                 ([name string?] [arity natural?] [widths (listof natural?)] [ast block?])
                 #:transparent
                 #:mutable)

(define/contract (ast-function-vars func)
  (-> ast-function? natural?)
  (length (ast-function-widths func)))

(provide (struct-out ast-function)
         ast-function-vars)
