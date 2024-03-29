#lang racket/base

(require racket/cmdline)
(require racket/function)
(require racket/list)
(require racket/string)
(require racket/format)
(require "src/c-frontend-cpp.rkt")
(require "src/c-frontend-lexer.rkt")
(require "src/c-frontend-parser.brag")
(require "src/c-frontend-lower.rkt")
(require "src/ast.rkt")
(require "src/c-backend.rkt")
(require "src/symb-exec.rkt")
(require "src/leakage.rkt")

(define lowered-path (make-parameter #f))
(define lowered-prefix (make-parameter ""))
(define symb-exec-funcs (make-parameter '()))
(define symb-exec-fuel (make-parameter 2000))
(define leakage-exec-funcs (make-parameter '()))
(define exec-funcs (make-parameter '()))

(define input-file
  (command-line
   #:program "clique"
   #:once-each ["--lowered-path" path "Write lowered c file to this path" (lowered-path path)]
   ["--lowered-prefix" prefix "Add this prefix to all lowered functions" (lowered-prefix prefix)]
   ["--fuel"
    fuel
    "Maximum number of statements to execute symbolically"
    (symb-exec-fuel (string->number fuel))]
   #:multi
   ["--exec"
    func
    args
    "Execute func with concrete inputs. Specify args as a space separated string of integers"
    (exec-funcs (cons (cons func args) (exec-funcs)))]
   ["--verify"
    func
    "Symbolically execute func with arbitrary inputs, checking safety assertions"
    (symb-exec-funcs (cons func (symb-exec-funcs)))]
   ["--leak-verify"
    func
    input
    "Symbolically execute func with arbitrary inputs and input set to a secret, looking for leakage violations"
    (leakage-exec-funcs (cons (cons func input) (leakage-exec-funcs)))]
   #:args (input-file)
   input-file))

(define functions
  (time (begin
          (displayln "Preprocessing sources...")
          (define pre-processed (c-pre-process input-file))
          (displayln "Tokenizing and parsing sources...")
          (define tokens (tokenize (open-input-string pre-processed)))
          (define parse-tree (parse input-file tokens))
          (displayln "Lowering sources...")
          (lower-parse-tree parse-tree))))

(when (lowered-path)
  (printf "Writing lowered c to \"~a\"...~n" (lowered-path))
  (define out (open-output-file (lowered-path) #:exists 'replace))
  (functions-to-c-file functions out (lowered-prefix))
  (close-output-port out))

(define (get-func name)
  (hash-ref functions name (lambda () (error 'no-function "Can't find function ~a" name))))

(for ([f (reverse (exec-funcs))])
  (define-values (name args) (values (car f) (cdr f)))
  (printf "~nExecuting function ~a with arguments (~a)...~n" name args)
  (define func (get-func name))
  (define args+ (map string->number (string-split args)))
  (unless (eq? (length args+) (ast-function-arity func))
    (error 'incorrect-args
           "Wrong number of arguments (~a) provided. Expected ~a"
           (length args+)
           (ast-function-arity func)))
  (for ([arg args+] [name (ast-function-args func)] [width (ast-function-widths func)])
    (printf "~a: ~a~n"
            name
            (~r arg #:base 16 #:min-width (truncate (ceiling (/ width 4))) #:pad-string "0")))

  (time (begin
          (define result (concrete-exec func args+))
          (define width (list-ref (ast-function-widths func) 0))
          (printf
           "result: ~a~n"
           (~r result #:base 16 #:min-width (truncate (ceiling (/ width 4))) #:pad-string "0")))))

(for ([name (reverse (symb-exec-funcs))])
  (printf "~nVerifying function ~a with ~a steps of fuel...~n" name (symb-exec-fuel))
  (define func (get-func name))
  (time (symb-exec func (symb-exec-fuel))))

(for ([f (reverse (leakage-exec-funcs))])
  (define-values (func-name secret-name) (values (car f) (cdr f)))
  (printf "~nLeakage verifying function ~a with ~a steps of fuel...~n" func-name (symb-exec-fuel))
  (define func (get-func func-name))
  (define secret-idx (+ 1 (index-of (ast-function-args func) secret-name)))
  (unless secret-idx
    (error 'no-arg "Function ~a doesn't have an argument \"~a\"" func-name secret-name))
  (time (symb-exec-product func
                           secret-idx
                           (list mul-leakage branch-leakage cache-leakage)
                           (symb-exec-fuel))))
