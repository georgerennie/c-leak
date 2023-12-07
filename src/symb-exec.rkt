#lang rosette/safe

(require "ast.rkt")
(require "leakage.rkt")
(require "c-backend.rkt")
(require rosette/lib/destruct)

(struct exec-state ([vars #:mutable] widths) #:transparent)

(struct stmt-nop stmt () #:transparent)

(define (bv-cast width old-width var)
  (cond
    [(< width old-width) (extract (- width 1) 0 var)]
    [(> width old-width) (zero-extend var (bitvector width))]
    [else var]))

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
             (assert (not (bvzero? cond)) (format "User assertion: ~a" (ast-to-c-str stmt)))
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

(define (conc-init-state func args)
  (define widths-list (ast-function-widths func))
  (define widths (vector->immutable-vector (list->vector widths-list)))

  (define (val idx)
    (if (and (> idx 0) (<= idx (length args))) (list-ref args (- idx 1)) 0))

  ; Initialise all variables to concrete bitvector values
  ; This isn't particularly clean. oh well...
  (define vars
    (list->vector (cdr (foldl (lambda (width acc)
                                (define i (car acc))
                                (define l (cdr acc))
                                (cons (+ i 1) (append l (list (bv (val i) width)))))
                              (cons 0 '())
                              widths-list))))

  (exec-state vars widths))

; Hacky... Couldn't work out how to make a new vec otherwise
(define (copy-vec vec)
  (list->vector (vector->list vec)))

(define (copy-state state)
  (exec-state (copy-vec (exec-state-vars state)) (exec-state-widths state)))

; Copy an initial execution state, switching out secret-idx for a fresh
; symbolic variable
(define (symb-init-state-pair other secret-idx)
  (define widths (exec-state-widths other))
  (define width (vector-ref widths secret-idx))
  (define vars+ (copy-vec (exec-state-vars other)))
  (vector-set! vars+ secret-idx (new-var width))
  (exec-state vars+ widths))

(define (vars-to-str vars func)
  (define arity (ast-function-arity func))
  (define vars+ (map bitvector->natural (take (cdr (vector->list vars)) arity)))
  (format "~a" vars+))

(define (symb-exec func fuel)
  ; Define initial state
  (define state (symb-init-state func))
  (define ast (ast-function-ast func))

  ; Define execution function
  (define (symb-exec-core state ast fuel)
    (assert (> fuel 0) "Ran out of fuel")
    (if (null? ast) (void) (symb-exec-core state (step state ast) (- fuel 1))))

  ; Run, verifying assertions
  (define cex (verify (symb-exec-core (copy-state state) ast fuel)))

  (unless (unsat? cex)
    (let ([state-vars-conc (evaluate (exec-state-vars state) cex)])
      (displayln "Found counterexample with the following inputs:")
      (printf "cex: ~a~n" (vars-to-str state-vars-conc func))
      (displayln "Rerunning with concrete inputs:")
      (define state-conc (exec-state state-vars-conc (exec-state-widths state)))
      (symb-exec-core state-conc ast fuel)))

  (when (unsat? cex)
    (displayln "VERIFIED: No counterexample found")))

(define (concrete-exec func args)
  ; Define initial state
  (define state (conc-init-state func args))
  (define ast (ast-function-ast func))

  ; Define execution function
  (define (symb-exec-core state ast)
    (if (null? ast) (exec-state-vars state) (symb-exec-core state (step state ast))))

  (define final-state (symb-exec-core state ast))
  (bitvector->natural (vector-ref final-state 0)))

(define (symb-exec-product func secret-idx contracts fuel)
  ; Define initial state
  (define s0 (symb-init-state func))
  (define s1 (symb-init-state-pair s0 secret-idx))
  (define ast (ast-function-ast func))

  (define (eval state)
    (lambda (idx) (vector-ref (exec-state-vars state) idx)))

  (define (check-contract contract eval-s0 eval-s1 stmt)
    (define leak-0 ((leakage-contract-eval contract) eval-s0 stmt))
    (define leak-1 ((leakage-contract-eval contract) eval-s1 stmt))
    (assert
     (equal? leak-0 leak-1)
     (format "Programs produced different leakage (~a, ~a) for contract \"~a\" on statement:~n~a"
             leak-0
             leak-1
             (leakage-contract-name contract)
             (ast-to-c-str stmt))))

  (define (check-contracts contracts eval-s0 eval-s1 stmt)
    (destruct contracts
              [(list) (void)]
              [(list* head tail)
               (check-contract head eval-s0 eval-s1 stmt)
               (check-contracts tail eval-s0 eval-s1 stmt)]))

  ; Define execution function
  (define (symb-exec-core s0 s1 ast fuel)
    (assert (> fuel 0) "Ran out of fuel")
    (define (body)
      (check-contracts contracts (eval s0) (eval s1) (car ast))
      (define ast-0 (step s0 ast))
      (define ast-1 (step s1 ast))
      (assert (equal? ast-0 ast-1)
              (format "Control flow diverged on statement:~n~a" (ast-to-c-str (car ast))))
      (symb-exec-core s0 s1 ast-0 (- fuel 1)))

    (if (null? ast) (void) (body)))

  (define cex (verify (symb-exec-core (copy-state s0) (copy-state s1) ast fuel)))

  (unless (unsat? cex)
    (let ([s0-vars-conc (evaluate (exec-state-vars s0) cex)]
          [s1-vars-conc (evaluate (exec-state-vars s1) cex)])
      (displayln "Found counterexample with the following inputs:")
      (printf "cex-0: ~a~n" (vars-to-str s0-vars-conc func))
      (printf "cex-1: ~a~n" (vars-to-str s1-vars-conc func))
      (displayln "Rerunning with concrete inputs:")
      (define s0-conc (exec-state s0-vars-conc (exec-state-widths s0)))
      (define s1-conc (exec-state s1-vars-conc (exec-state-widths s1)))
      (symb-exec-core s0-conc s1-conc ast fuel)))

  (when (unsat? cex)
    (displayln "VERIFIED: No counterexample found")))

(provide symb-exec
         concrete-exec
         symb-exec-product)
