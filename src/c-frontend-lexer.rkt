#lang racket/base

(require brag/support)
(require br-parser-tools/lex)

(define (tokenize port)
  (port-count-lines! port)
  (define (next-token)
    (define whileleak-lexer
      (lexer-src-pos
       [(:or "typedef" "while" "for" "if" "const" "static" "return" "assert") lexeme]
       [(:: (char-set "+*&|^") "=") lexeme]
       [(:or "<<" ">>" "<<=" ">>=" "==") lexeme]
       [(char-set "[]{}()=;+*<>,&|^") lexeme]
       ["false" (token 'INT 0)]
       ["true" (token 'INT 1)]
       ; Match binary consts
       [(:: "0b" (:+ (char-set "01"))) (token 'INT (string->number (substring lexeme 2) 2))]
       [(:: "0b" (:+ (char-set "01")) (:or "u" "U"))
        (token 'INT (string->number (substring lexeme 2 (- (string-length lexeme) 1)) 2))]
       ; Match hex consts
       [(:: "0x" (:+ (:or numeric (char-set "abcdefABCDEF"))))
        (token 'INT (string->number (substring lexeme 2) 16))]
       [(:: "0x" (:+ (:or numeric (char-set "abcdefABCDEF"))) (:or "u" "U"))
        (token 'INT (string->number (substring lexeme 2 (- (string-length lexeme) 1)) 16))]
       ; Only support unsigned decimals
       [(:: (:+ numeric) (:or "u" "U"))
        (token 'INT (string->number (substring lexeme 0 (- (string-length lexeme) 1))))]
       [(:: (:or alphabetic "_") (:* (:or alphabetic "_" numeric))) (token 'ID lexeme)]
       [whitespace (next-token)]))
    (whileleak-lexer port))
  next-token)

(provide tokenize)
