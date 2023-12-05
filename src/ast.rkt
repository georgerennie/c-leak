#lang racket/base

(require racket/math)
(require racket/contract/base)
(require racket/contract/region)
(require racket/match)

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
(struct/contract assign stmt ([lhs natural?] [op op?]) #:transparent)
(struct/contract while
                 stmt
                 ((cond
                    natural?)
                  [block block?])
                 #:transparent)

(provide (struct-out stmt)
         block?
         (struct-out assign)
         (struct-out while))

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
                 ([name string?] [arity natural?] [widths (hash/c natural? natural?)] [ast block?])
                 #:transparent
                 #:mutable)

(define/contract (ast-function-vars func)
  (-> ast-function? natural?)
  (hash-count (ast-function-widths func)))

(provide (struct-out ast-function)
         ast-function-vars)

(define/contract (var-to-str var)
  (-> natural? string?)
  (format "v~a" var))

(define/contract (const-to-str c)
  (-> natural? string?)
  (format "~au" c))

(define/contract (op-to-c op port)
  (-> op? output-port? void?)
  (match op
    [(un-op a)
     (cond
       [(op-id? op) (display (var-to-str a) port)]
       [(op-const? op) (display (const-to-str a) port)])]
    [(bin-op a b)
     (fprintf port
              "~a ~a ~a"
              (var-to-str a)
              (cond
                [(op-add? op) "+"]
                [(op-mul? op) "*"]
                [(op-and? op) "&"]
                [(op-or? op) "|"]
                [(op-xor? op) "^"]
                [(op-sll? op) "<<"]
                [(op-srl? op) ">>"]
                [(op-lt? op) "<"]
                [(op-gt? op) ">"])
              (var-to-str b))]
    [(op-lut idx lut)
     (display
      (for/fold ([str (const-to-str 0)]) ([item lut] [i (in-naturals)])
        (format "(~a == ~a) ? ~a : (~a)" (var-to-str idx) (const-to-str i) (const-to-str item) str))
      port)])
  (void))

(define/contract (stmt-to-c stmt port [prefix ""])
  (->* [stmt? output-port?] [string?] void?)
  (match stmt
    [(assign lhs op)
     (fprintf port "~a~a = " prefix (var-to-str lhs))
     (op-to-c op port)
     (displayln ";" port)]
    [(while cond block)
     (fprintf port "~awhile (~a) {~n" prefix (var-to-str cond))
     (block-to-c block port (string-append prefix "\t"))
     (fprintf port "~a}~n" prefix)])
  (void))

(define/contract (block-to-c block port [prefix ""])
  (->* [block? output-port?] [string?] void?)
  (for ([stmt block])
    (stmt-to-c stmt port prefix))
  (void))

(define/contract (c-uint-str width)
  (-> natural? string?)
  (case width
    [(0) "void"]
    [(1) "bool"]
    [(8) "uint8_t"]
    [(16) "uint16_t"]
    [(32) "uint32_t"]
    [(64) "uint64_t"]
    [else (format "_BitInt(~a)" width)]))

(define/contract (function-to-c func port)
  (-> ast-function? output-port? void?)
  (define ret-width (hash-ref (ast-function-widths func) 0))
  (define non-void-return (positive? ret-width))
  (define arity (ast-function-arity func))

  (fprintf port "~a ~a(~n" (c-uint-str ret-width) (ast-function-name func))

  (for ([i (in-inclusive-range 1 arity)])
    (define delim (if (eq? i arity) "" ","))
    (define width (hash-ref (ast-function-widths func) i))
    (fprintf port "\t~a ~a~a~n" (c-uint-str width) (var-to-str i) delim))

  (displayln ") {" port)

  (when non-void-return
    (fprintf port "\t~a ~a;~n" (c-uint-str ret-width) (var-to-str 0)))

  (for ([i (in-range (+ arity 1) (ast-function-vars func))])
    (define width (hash-ref (ast-function-widths func) i))
    (fprintf port "\t~a ~a;~n" (c-uint-str width) (var-to-str i)))

  (block-to-c (ast-function-ast func) port "\t")
  (when non-void-return
    (fprintf port "\treturn ~a;~n" (var-to-str 0)))
  (displayln "}" port)
  (void))

(define/contract (ast-to-c ast port)
  (-> (or/c ast-function? block? stmt? op?) output-port? void?)
  (cond
    [(ast-function? ast) (function-to-c ast port)]
    [(block? ast) (block-to-c ast port)]
    [(stmt? ast) (stmt-to-c ast port)]
    [(op? ast) (op-to-c ast port)])
  (void))

(define/contract (functions-to-c-file functions port [prefix ""])
  (-> (hash/c string? ast-function?) output-port? string? void?)
  (displayln "#include <stdint.h>" port)
  (displayln "#include <stdbool.h>" port)
  (hash-for-each
   functions
   (lambda (_ f)
     (displayln "" port)
     (define f+ (struct-copy ast-function f [name (string-append prefix (ast-function-name f))]))
     (function-to-c f+ port)))
  (void))

(provide ast-to-c
         functions-to-c-file)
