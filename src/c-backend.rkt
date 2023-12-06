#lang racket/base

(require "ast.rkt")
(require racket/contract/base)
(require racket/contract/region)
(require racket/match)
(require racket/math)
(require racket/list)

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
    [(stmt-assign lhs op)
     (fprintf port "~a~a = " prefix (var-to-str lhs))
     (op-to-c op port)
     (displayln ";" port)]
    [(stmt-while cond body)
     (fprintf port "~awhile (~a) {~n" prefix (var-to-str cond))
     (block-to-c body port (string-append prefix "\t"))
     (fprintf port "~a}~n" prefix)]
    [(stmt-if cond body)
     (fprintf port "~aif (~a) {~n" prefix (var-to-str cond))
     (block-to-c body port (string-append prefix "\t"))
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
  (define ret-width (car (ast-function-widths func)))
  (define non-void-return (positive? ret-width))
  (define arity (ast-function-arity func))

  (fprintf port "~a ~a(~n" (c-uint-str ret-width) (ast-function-name func))

  (define-values (args local-vars) (split-at (cdr (ast-function-widths func)) arity))

  (define (print-args args [i 1])
    (match args
      [(list width) (fprintf port "\t~a ~a~n" (c-uint-str width) (var-to-str i))]
      [(list* width tail)
       (fprintf port "\t~a ~a,~n" (c-uint-str width) (var-to-str i))
       (print-args tail (+ i 1))]
      [_ (void)]))
  (print-args args)

  (displayln ") {" port)

  (when non-void-return
    (fprintf port "\t~a ~a;~n" (c-uint-str ret-width) (var-to-str 0)))

  (for ([i (in-naturals)] [width local-vars])
    (fprintf port "\t~a ~a;~n" (c-uint-str width) (var-to-str (+ arity 1 i))))

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
