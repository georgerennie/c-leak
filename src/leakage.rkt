#lang rosette/safe

(require "ast.rkt")
(require rosette/lib/destruct)

(struct leakage-contract (name eval) #:transparent)

(define branch-leakage
  (leakage-contract "branch"
                    (lambda (eval stmt)
                      (destruct stmt
                                [(stmt-while cond-idx _) (bvzero? (eval cond-idx))]
                                [(stmt-if cond-idx _) (bvzero? (eval cond-idx))]
                                [_ (void)]))))

(define mul-leakage
  (leakage-contract "multiplication"
                    (lambda (eval stmt)
                      (destruct stmt
                                [(stmt-assign _ op)
                                 (destruct op [(op-mul a b) (cons (eval a) (eval b))] [_ (void)])]
                                [_ (void)]))))

(define cache-leakage
  (leakage-contract "cache"
                    (lambda (eval stmt)
                      (destruct stmt
                                [(stmt-assign _ op)
                                 (destruct op [(op-lut idx _) (eval idx)] [_ (void)])]
                                [_ (void)]))))

(provide (struct-out leakage-contract)
         branch-leakage
         mul-leakage
         cache-leakage)
