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
                            [(op-eq? op) (bool->bitvector (bveq a b))]
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
            [(stmt-assert cond-idx)
             (define cond (vector-ref vars cond-idx))
             (assert (not (bvzero? cond)) "User assertion")
             tail]
            [(stmt-nop) tail]))

(define (new-var width)
  (define width+ (max 1 width))
  (define-symbolic* var (bitvector width+))
  var)

(define (symb-init-state func)
  (define widths-list (ast-function-widths func))
  (define widths (vector->immutable-vector (list->vector widths-list)))

  ; Initialise all variables to symbolic values
  (define vars (list->vector (map new-var widths-list)))

  (exec-state vars widths))

(define (symb-init-state-pair other secret-idx)
  (define widths (exec-state-widths other))
  (define width (vector-ref widths secret-idx))
  ; Hacky... Couldn't work out how to make a new vec otherwise
  (define vars+ (list->vector (vector->list (exec-state-vars other))))
  (vector-set! vars+ secret-idx (new-var width))
  (exec-state vars+ widths))

(define (symb-exec func fuel)
  ; Define initial state
  (define state (symb-init-state func))
  (define ast (ast-function-ast func))

  ; Define execution function
  (define (symb-exec-core state ast fuel)
    (assert (> fuel 0) "Ran out of fuel")
    (if (null? ast) (void) (symb-exec-core state (step state ast) (- fuel 1))))

  ; Run, verifying assertions
  (displayln (verify (symb-exec-core state ast fuel))))

(define (symb-exec-product func secret-idx fuel)
  ; Define initial state
  (define s0 (symb-init-state func))
  (define s1 (symb-init-state-pair s0 secret-idx))
  (define ast (ast-function-ast func))

  ; Define execution function
  (define (symb-exec-core s0 s1 ast fuel)
    (assert (> fuel 0) "Ran out of fuel")
    (if (null? ast)
        (void)
        (let ([ast-0 (step s0 ast)] [ast-1 (step s1 ast)])
          (assert (equal? ast-0 ast-1) "Control flow shouldn't diverge")
          (symb-exec-core s0 s1 ast-0 (- fuel 1)))))

  (displayln (verify (symb-exec-core s0 s1 ast fuel))))

(provide symb-exec
         symb-exec-product)
