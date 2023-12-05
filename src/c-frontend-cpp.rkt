#lang racket/base

(require racket/match)
(require racket/path)
(require racket/sequence)
(require racket/list)

; minimal C pre-processor with the following features:
; - removes line comments
; - resolves local includes ("file.h")
; - ignores system includes (<file.h>)
; - ignores pragmas
;
; TODO: currently doesn't check for recursive imports and ignores pragma once
; This could probably be written in a more performant/functional etc way but for
; the small programs I want to use it should suffice for now

(define (expand-directive directive file-path)
  (define path-str (if (path? file-path) (path->string file-path) file-path))
  (match directive
    ; Resolve local includes
    [(pregexp #px"^include[[:space:]]*\"([^\"]+)\"[[:space:]]*$" (list _ fn))
     (define rel-path (path->complete-path (path-only (normalize-path file-path))))
     (c-pre-process (normalize-path fn rel-path))]
    [(pregexp #px"^(pragma|include[[:space:]]*<)")
     (printf "CPP: Ignoring #~a in ~a~n" directive path-str)
     ""]
    [_ (error 'expand-directive "Unexpected CPP directive \"#~a\" in ~a" directive path-str)]))

(define (c-pre-process path)
  (unless (file-exists? path)
    (error 'cpp-unknown-file "Can't find file ~a" path))
  (define in-port (open-input-file path #:mode 'text))
  (sequence-fold (lambda (acc line)
                   ; Remove line comments
                   (define no-comment (regexp-replace #rx"//.*" line ""))
                   (define directive (regexp-match #px"^[[:space:]]*#(.*)" no-comment))
                   (if directive
                       (string-append acc (expand-directive (second directive) path) "\n")
                       (string-append acc no-comment "\n")))
                 (string)
                 (in-lines in-port)))

(provide c-pre-process)
