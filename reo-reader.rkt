#lang br/quicklang
(require "reo-parser.rkt")

(define (read-syntax path port)
  (displayln 'start-reading)
  (define tokenizer (make-tokenizer port))
  ;; (for ([i (range 60)]) (displayln (tokenizer)))
  (displayln 'start-parsing)
  (define parse-tree (parse path tokenizer))
  (define module-datum `(module reo-mod "reo-expander.rkt"
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)

(require brag/support)

(define-lex-abbrev reserved-terms (:or "a" "b" "c" "p1c" "p2c"))
(define (make-tokenizer port)
  (define (next-token)
    (define bf-lexer
      (lexer
       ["\n" (token 'NEWLINE lexeme)]
       ["," (token 'SEP lexeme)]
       [reserved-terms (token lexeme lexeme)]
       [(:+ (:or alphabetic numeric (char-set "-_\""))) (token 'DATA lexeme)]
       [(:or (char-set ":()") whitespace) (token lexeme #:skip? #t)]))
  (bf-lexer port))
  next-token)
