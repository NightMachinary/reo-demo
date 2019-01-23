#lang racket

(require srfi/26)
(define null "null")
(define (knull? x)
  (equal? null x))
(define raise raise-arguments-error)
(define (cls x) (filter identity x))
(define (merger x y)
  (cls
   (apply
    append
    (map
     (λ (x)
       (map
        (λ (y)
          (with-handlers([exn:fail? (λ (x)#f)])
            (map (λ (x y)
                   (if (or (knull? x) (knull? y))
                       (if (knull? x) y x)
                       (raise 'merger
                              #t)))
                 x y)))
        y))
     x))))
  
  
(define (sync x) x)
(define (sync-drain x y)
  (cls
   (apply
    append
    (map
     (λ (x)
       (map
        (λ (y)
            
          (with-handlers([exn:fail? (λ (x)#f)])
            (map
             (λ (x y)
               (let ([nx (knull? x)]
                     [ny (knull? y)])
                 (if (xor nx ny)
                     (raise 'sync-drain
                            #t)
                     x)))
             x y)))
        y))
     x))))

(define (join x y)
  (cls
   (map
    (λ (x)
      (map
       (λ (y)
         (with-handlers([exn:fail? (λ (x)#f)])
           (map (λ (x y)
                  (let ([nx (knull? x)]
                        [ny (knull? y)])
                    (if (xor nx ny)
                        (raise 'join
                               #t)
                        (if nx null (cons x y)))))
                x y)))
       y))
    x)))

(define (all x n state acc)
  (cond
    [(zero? n) (cons acc '())]
    [else
     (let ([ax (car x)]
           [dx (cdr x)])
       (if (knull? ax)
           (if (knull? state)
               (all dx (sub1 n) state (cons null acc))
               (append
                
                (all dx (sub1 n) state (cons null acc))
                (all dx (sub1 n) null (cons state acc))))
           (all dx (sub1 n) ax (cons state acc))))]))
(define (rall x)
  (map reverse (all x (length x) null '())))

(define (fifo1 x)
  (apply append
         (map rall 
              x)))



(define (p1 a b) (sync-drain a b))
(define (p2 a b) (merger
  (sync-drain a b)
  (fifo1 ( sync-drain b a))))


(define (p1c a b c) (ormap (cute equal? c <>) (p1 (list a) (list b))))
(define (p2c a b c) (ormap (cute equal? c <>) (p2 (list a) (list b))))

(provide p1c p2c)

(define a1 (list (list 1 null 100 -10 0)))
(define b1 (list (list 1 null -200 -15 20)))
(define c1 (car a1))

(define a2 (list (list "hello" null "hello" null null)))
(define b2 (list (list "world" null "world" null null)))
(define c2 (list "hello" "world" "hello" "world" null))

(ormap (cute equal? c2 <>) (p2 a2 b2))
(ormap (cute equal? c1 <>) (p1 a1 b1))

