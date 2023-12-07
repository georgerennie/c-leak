#lang racket/base

(require racket/math)
(require racket/contract/base)
(require racket/contract/region)
(require racket/match)
(require racket/function)

(require "ast.rkt")

; Parse state keeps track of the general state as we process the parsed input
; types     - an index from name to width of uint types currently defined
; var-idxs  - a (reverse) list of scopes, where each contains a hash from
;             variable ids to their corresponding variable indices in the
;             current function
; functions - a map from function names to ast-function objects for previously
;             declared functions
; luts      - a map from names of lookup tables (global const arrays) to a pair
;             of their variable width and values
; This state is updated mutably as we go
(struct/contract
 parse-state
 ([types (hash/c string? natural?)] [var-idxs (listof (hash/c string? natural?))]
                                    [functions (hash/c string? ast-function?)]
                                    [luts (hash/c string? (cons/c natural? (listof natural?)))])
 #:transparent
 #:mutable)

; When parsing blocks/functions, this tracks both the global parse state and
; the state for the current function (see ast.rkt). Note that the actual ast of
; statements is not updated in func as we go, but instead passed around
; functionally, and updated into func when a function has been fully processed
(struct/contract block-state ([parse parse-state?] [func ast-function?]) #:transparent #:mutable)

; Create/end a new scope for variables
(define/contract (new-scope state)
  (-> (or/c parse-state? block-state?) void?)
  (define pstate (if (parse-state? state) state (block-state-parse state)))
  (set-parse-state-var-idxs! pstate (cons (make-hash) (parse-state-var-idxs pstate)))
  (void))

(define/contract (end-scope state)
  (-> (or/c parse-state? block-state?) void?)
  (define pstate (if (parse-state? state) state (block-state-parse state)))
  (set-parse-state-var-idxs! pstate (cdr (parse-state-var-idxs pstate)))
  (void))

(define/contract (add-var state
                          width
                          #:name [name ""]
                          #:idx [idx (ast-function-vars (block-state-func state))])
  (->* (block-state? natural?) (#:name string? #:idx natural?) natural?)
  ; Add to parse state
  (unless (eq? name "")
    (define scope (car (parse-state-var-idxs (block-state-parse state))))
    (hash-set! scope name idx))
  ; Add to function
  (define func (block-state-func state))
  (define widths (ast-function-widths func))
  (set-ast-function-widths! func (append widths (list width)))
  idx)

(define/contract (var-idx var-name state)
  (-> string? parse-state? natural?)
  (define h (findf (lambda (h) (hash-has-key? h var-name)) (parse-state-var-idxs state)))
  (unless h
    (error 'var-idx "Variable ~a has not been declared in this scope" var-name))
  (hash-ref h var-name))

(define default-types
  '(("void" . 0) ("bool" . 1)
                 ("uint8_t" . 8)
                 ("uint16_t" . 16)
                 ("uint32_t" . 32)
                 ("uint64_t" . 64)))

(define/contract (prog . ast)
  (->* () #:rest (listof procedure?) (hash/c string? ast-function?))
  (define state (parse-state (make-hash default-types) '() (make-hash) (make-hash)))
  (for ([stmt ast])
    (stmt state))
  (parse-state-functions state))

(define/contract (global-stmt . ast)
  (->* () #:rest (listof any/c) (-> parse-state? void?))
  (lambda (state)
    (match ast
      [(list "typedef" type id) (hash-set! (parse-state-types state) id (type state))]
      [(list f) (f state)])))

(define/contract (func-sig . ast)
  (->* () #:rest (listof any/c) (-> parse-state? void?))
  (match ast
    [(list _ id ...)
     (printf "NOTE: Ignoring function declaration ~a. Use C compiler to check correctness~n"
             (car id))])
  (const (void)))

(define/contract (lut-decl type id len . vals)
  (->* (procedure? string? natural?) #:rest (listof natural?) (-> parse-state? void?))
  (unless (eq? (length vals) len)
    (error 'incorrect-lut-length
           "Lut ~a declared with length ~a but given ~a initialisers"
           id
           len
           (length vals)))
  (lambda (state) (hash-set! (parse-state-luts state) id (cons (type state) vals))))

(define/contract (function . ast)
  (->* () #:rest (listof any/c) (-> parse-state? void?))
  (match ast
    [(list ret-type name params block)
     (lambda (state)
       (new-scope state)

       (define func (ast-function name '() '() '()))
       (define state+ (block-state state func))
       (add-var state+ (ret-type state) #:idx 0)

       ; Add params
       (define params+
         (for/list ([param params] [i (in-naturals)])
           (define-values (name width) (param state))
           (add-var state+ width #:name name #:idx (+ i 1))
           name))
       (set-ast-function-args! func params+)

       ; Analyse block
       (set-ast-function-ast! func (block state+))

       ; Add function
       (hash-set! (parse-state-functions state) name func)

       (end-scope state)
       (void))]))

(define (param-list . ast)
  ast)

(define/contract (var-decl . ast)
  (-> procedure? string? (-> parse-state? (values string? natural?)))
  (match ast
    [(list type name) (lambda (state) (values name (type state)))]))

(define/contract (type . ast)
  (-> string? (-> parse-state? natural?))
  (lambda (state)
    (match ast
      [(list id)
       (hash-ref (parse-state-types state)
                 id
                 (lambda () (error 'type "Unknown type \"~a\"" id)))])))

(define (fn-block . ast)
  (apply block ast))

(define/contract (block . ast)
  (->* () #:rest (listof (-> block-state? block?)) (-> block-state? block?))
  (lambda (state)
    (for/fold ([ast '()]) ([stmt ast])
      (append ast (stmt state)))))

; Following 6.4.4.1 of C spec for unsigned (u) integer constants, taking
; unsigned int as 4 bytes and unsigned long (long) int as 8 bytes. Anything
; greater is unsupported
(define/contract (const-width val)
  (-> natural? natural?)
  (define bits (if (eq? 0 val) 1 (truncate (ceiling (log val 2)))))
  (cond
    [(<= bits 32) 32]
    [(<= bits 64) 64]
    [else (error 'const-width "Constant value ~a is wider than any supported type" val)]))

(define/contract (expr . ast)
  ; Returns lowered ast and var idx of last variable
  (->* ()
       #:rest (listof any/c)
       (->* (block-state?) ((or/c boolean? natural?)) (values block? natural?)))
  ; If assign-to is a variable index, assign to that variable instead of a fresh
  ; var (or no assignment in the case of an id reference)
  (lambda (state [assign-to #f])
    (match ast
      ; Reference to existing variable
      [(list id)
       #:when (string? id)
       (define var (var-idx id (block-state-parse state)))
       (values (if assign-to (list (stmt-assign assign-to (op-id var))) '()) (or assign-to var))]
      ; Constant value
      [(list int)
       #:when (natural? int)
       (define op (op-const int))
       (define width (const-width int))
       (define lhs (or assign-to (add-var state width)))
       (values (list (stmt-assign lhs op)) lhs)]
      ; Function call
      [(list func) (func state assign-to)]
      ; Standard binary operator
      [(list a op b)
       (define-values (a-ast a-var) (a state))
       (define-values (b-ast b-var) (b state))
       (define op+
         ((case op
            [("+") op-add]
            [("*") op-mul]
            [("&") op-and]
            [("|") op-or]
            [("^") op-xor]
            [("<<") op-sll]
            [(">>") op-srl]
            [("==") op-eq]
            [("<") op-lt]
            [(">") op-gt])
          a-var
          b-var))
       (define widths (ast-function-widths (block-state-func state)))
       (define a-width (list-ref widths a-var))
       (define b-width (list-ref widths b-var))
       (define width (max 32 a-width b-width))
       (define lhs (or assign-to (add-var state width)))
       (values (append a-ast b-ast (list (stmt-assign lhs op+))) lhs)]
      ; LUT lookup
      [(list id "[" expr "]")
       (define-values (expr-ast expr-var) (expr state))
       (define lut (hash-ref (parse-state-luts (block-state-parse state)) id))
       (define-values (width vals) (values (car lut) (cdr lut)))
       (define lhs (or assign-to (add-var state width)))
       (values (append expr-ast (list (stmt-assign lhs (op-lut expr-var vals)))) lhs)])))

; When inlining existing functions into the current one, this shifts all variable
; indices up by base so they don't overlap
(define/contract (rebase-ast ast base)
  (-> block? natural? block?)
  (for/list ([stmt ast])
    (match stmt
      [(stmt-assign lhs op)
       (define op+
         (match op
           [(op-id a) (op-id (+ base a))]
           [(bin-op a b)
            (define-values (a+ b+) (values (+ base a) (+ base b)))
            (cond
              [(op-add? op) (op-add a+ b+)]
              [(op-mul? op) (op-mul a+ b+)]
              [(op-and? op) (op-and a+ b+)]
              [(op-or? op) (op-or a+ b+)]
              [(op-xor? op) (op-xor a+ b+)]
              [(op-sll? op) (op-sll a+ b+)]
              [(op-srl? op) (op-srl a+ b+)]
              [(op-eq? op) (op-eq a+ b+)]
              [(op-lt? op) (op-lt a+ b+)]
              [(op-gt? op) (op-gt a+ b+)])]
           [(op-lut idx lut) (op-lut (+ base idx) lut)]
           [_ op]))
       (stmt-assign (+ base lhs) op+)]
      [(stmt-while cond body) (stmt-while (+ base cond) (rebase-ast body base))]
      [(stmt-if cond body) (stmt-if (+ base cond) (rebase-ast body base))]
      [(stmt-assert cond) (stmt-assert (+ base cond))])))

(define/contract (func-call . ast)
  ; Same types as expr
  (->* ()
       #:rest (listof any/c)
       (->* (block-state?) ((or/c boolean? natural?)) (values block? natural?)))
  (match ast
    [(list* id args)
     (lambda (state [assign-to #f])
       ; Calculate value of all arguments
       (define-values (args-ast args-vars)
         (for/fold ([args-ast '()] [args-vars '()]) ([arg args])
           (define-values (ast var) (arg state))
           (values (append args-ast ast) (append args-vars (list var)))))
       ; Now arguments have been calculated, we can start the function's
       ; variable indices at the next free value
       (define base (ast-function-vars (block-state-func state)))

       ; Assign the arguments into the function's equivalent variables
       (define asn-args-ast
         (for/list ([var args-vars] [i (in-naturals)])
           (stmt-assign (+ base i 1) (op-id var))))

       ; Process function so that it's variable indices are shifted
       (define func (hash-ref (parse-state-functions (block-state-parse state)) id))
       (define func-ast (rebase-ast (ast-function-ast func) base))
       (define func-widths (ast-function-widths func))

       ; Add variable widths to caller state
       (set-ast-function-widths! (block-state-func state)
                                 (append (ast-function-widths (block-state-func state))
                                         func-widths))

       ; Assign return variable back out of inlined function
       (define ret-width (car func-widths))
       (define ret-var (or assign-to (add-var state ret-width)))
       (define asn-ret (list (stmt-assign ret-var (op-id base))))

       (values (append args-ast asn-args-ast func-ast asn-ret) ret-var))]))

(define/contract (stmt . ast)
  (->* () #:rest (listof any/c) (-> block-state? block?))
  (lambda (state)
    (match ast
      [(list "while" cond-expr body)
       (define-values (cond-ast cond-var) (cond-expr state))
       (new-scope state)
       (define body-ast (body state))
       (end-scope state)
       (append cond-ast (list (stmt-while cond-var (append body-ast cond-ast))))]
      ; for (a; b; c) { d } desugars to
      ; a; while (b) { d; c }
      [(list "for" type id "=" init-expr cond-expr inc-id inc-op inc-expr body)
       ((block (stmt type id "=" init-expr)
               (stmt "while"
                     cond-expr
                     (lambda (state+)
                       (define body-ast (body state+))
                       (define inc-ast ((stmt inc-id inc-op inc-expr) state+))
                       (append body-ast inc-ast))))
        state)]
      [(list "if" cond-expr block)
       (define-values (cond-ast cond-var) (cond-expr state))
       (new-scope state)
       (define body-ast (block state))
       (end-scope state)
       (append cond-ast (list (stmt-if cond-var body-ast)))]
      [(list "assert" cond-expr)
       (define-values (cond-ast cond-var) (cond-expr state))
       (append cond-ast (list (stmt-assert cond-var)))]
      [(list id "=" expr)
       (define var (var-idx id (block-state-parse state)))
       (define-values (ast _) (expr state var))
       ast]
      ; De-sugar modify-assign operators into assign with expr on rhs
      [(list id assign-op rhs)
       (define op-str (substring assign-op 0 (- (string-length assign-op) 1)))
       (define rhs+ (expr (expr id) op-str rhs))
       ((stmt id "=" rhs+) state)]
      [(list type id "=" expr)
       (define width (type (block-state-parse state)))
       (define var (add-var state width #:name id))
       (define-values (ast _) (expr state var))
       ast])))

(define/contract (return . ast)
  (-> string? procedure? (-> block-state? block?))
  (match ast
    [(list "return" expr)
     ; Return is equivalent to doing v0 = expr;
     (lambda (state)
       (define-values (ast _) (expr state 0))
       ast)]))

(define-namespace-anchor anc)

; Lowers a parse tree generated by c-frontend-parser.rkt to a set of
; ast-functions from ast.rkt labelled by function name
(define/contract (lower-parse-tree parse-tree)
  (-> syntax? (hash/c string? ast-function?))
  (define ns (namespace-anchor->namespace anc))
  (eval parse-tree ns))

(provide lower-parse-tree)
