#lang br/quicklang
(require (only-in racket display-to-file))

(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     'PARSE-TREE
     PARSE-TREE
     ))
(provide (rename-out [bf-module-begin #%module-begin]) p1-program displayln p1c p2c #%top #%top-interaction #%datum)

(require "reo-backend.rkt")

(define-macro (p1-program NET OUTPUT PROG ...)
  
  #'(begin
      (displayln 'PROG) ...
      (displayln 'hmm)
      (displayln '(PROG ...))
      (run-p NET OUTPUT '(PROG ...))) 
  )
;; (define my-ns (namespace-attach-module (make-base-namespace) "reo-backend.rkt"))
(define (run-p network output prog)
  (to-out output (map (curry apply
                             (if (equal? network "p1c") p1c p2c)) prog)))

(define (to-out output res)
  (display-to-file (my-list->text res) output #:exists 'truncate/replace))
(define (my-list->text l)
  (let tt ([l l] [res ""])
    (if (null? l)
        res
        (tt (cdr l) (~a res (if (car l) "True" "False") "\n")))))
;; (displayln 'hi)
;; (p1c '("1" "null" "100" "-10" "0") '("1" "null" "-200" "-15" "20") '("1" "null" "100" "-10" "0"))

