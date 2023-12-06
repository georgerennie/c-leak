#lang rosette/safe

(require "ast.rkt")
(require rosette/lib/destruct)

(struct exec-state ([vars #:mutable] widths) #:transparent)

(struct stmt-nop stmt () #:transparent)

(define (bv-cast width old-width var)
  (if (<= width old-width) (extract (- width 1) 0 var) (zero-extend var (bitvector width))))

(define (step state ast)
  (define stmt (car ast))
  (define tail (cdr ast))
  (define vars (exec-state-vars state))
  (define widths (exec-state-widths state))
  (destruct stmt
            [(stmt-assign lhs op)
             (define lhs-width (vector-ref widths lhs))
             (define (set val width)
               ; Handle assignment to bools
               (define val+
                 (if (equal? lhs-width 1)
                     (bool->bitvector (not (bvzero? val)))
                     (bv-cast lhs-width width val)))
               (vector-set! vars lhs val+))
             (destruct op
                       [(op-lut idx lut)
                        (define width (max 32 lhs-width))
                        ; TODO: Constructing this each time isn't amazing for
                        ; perf
                        (define bv-lut (map (lambda (t) (bv t width)) lut))
                        (define var (vector-ref vars idx))
                        (set (list-ref-bv bv-lut var) width)]
                       [(op-id idx) (set (vector-ref vars idx) (vector-ref widths idx))]
                       [(op-const val) (define width (max 32 lhs-width)) (set (bv val width) width)]
                       [(bin-op a-idx b-idx)
                        (define a-width (vector-ref widths a-idx))
                        (define b-width (vector-ref widths b-idx))
                        (define op-width (max 32 a-width b-width))
                        (define a (bv-cast op-width a-width (vector-ref vars a-idx)))
                        (define b (bv-cast op-width b-width (vector-ref vars b-idx)))
                        (define res-width (if (bool-op? op) 1 op-width))
                        (define val
                          (cond
                            [(op-add? op) (bvadd a b)]
                            [(op-mul? op) (bvmul a b)]
                            [(op-and? op) (bvand a b)]
                            [(op-or? op) (bvor a b)]
                            [(op-xor? op) (bvxor a b)]
                            [(op-sll? op) (bvshl a b)]
                            [(op-srl? op) (bvlshr a b)]
                            [(op-lt? op) (bool->bitvector (bvult a b))]
                            [(op-gt? op) (bool->bitvector (bvugt a b))]))
                        (set val res-width)])
             tail]
            [(stmt-while cond-idx body)
             (define cond (vector-ref vars cond-idx))
             (if (bvzero? cond) tail (append body ast))]
            ; Hacky way of treating if statements that gives better symb exec
            ; performance - always execute the body but convert it to no-ops
            ; if needed
            [(stmt-if cond-idx body)
             (define cond (vector-ref vars cond-idx))
             (define gated-expr (map (lambda (s) (if (bvzero? cond) (stmt-nop) s)) body))
             (append gated-expr tail)]
            [(stmt-nop) tail]))

(define (symb-exec func)
  ; Setup initial state
  (define widths-list (ast-function-widths func))
  (define widths (vector->immutable-vector (list->vector widths-list)))

  ; Initialise all variables to symbolic values
  (define vars
    (list->vector (map (lambda (width)
                         (define-symbolic* var (bitvector width))
                         var)
                       widths-list)))

  (define state (exec-state vars widths))
  (define ast (ast-function-ast func))

  ; Define execution function
  (define (symb-exec-core state ast [fuel 1000])
    (assert (> fuel 0) "Ran out of fuel")
    (define ast+ (step state ast))
    (if (null? ast+) (exec-state-vars state) (symb-exec-core state ast+ (- fuel 1))))

  ; Run, verifying assertions
  (displayln (verify (symb-exec-core state ast))))

(provide symb-exec)
