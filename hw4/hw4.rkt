#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence spacing low high)
  (if (> low high)
      null
      (cons low (sequence spacing (+ low spacing) high)))
  )

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs)
  )

(define (list-nth-mod xs n)
  (letrec ([l (length xs)]
           [f (lambda (i lst) (if (= i 0) (car lst) (f (- i 1) (cdr lst))))])
    (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (f (remainder n l) xs)))))

(define (stream-for-k-steps s k)
  (letrec ([pr (s)])
    (if (= k 0)
        null
        (cons (car pr) (stream-for-k-steps (cdr pr) (- k 1))))))

(define (zero-through-n n)
  (letrec ([f (lambda (x y) (cons (modulo y x) (lambda () (f x (+ y 1)))))])
    (lambda () (f (+ n 1) 0))
   ))

(define funny-number-stream
  (letrec ([next (lambda (x) (if (= (remainder x 6) 0)
                                     (cons (* -1 x) (lambda () (next (+ x 1))))
                                     (cons x (lambda () (next (+ x 1))))))])
   (lambda () (next 1))))

(define dan-then-dog
  (letrec ([f (lambda () (cons "dan.jpg" (lambda () (g))))]
           [g (lambda () (cons "dog.jpg" (lambda () (f))))])
    (lambda () (f))
   ))

(define (stream-add-one s)
  (letrec ([f (lambda (s) (letrec ([pr (s)])
                            (cons (cons 1 (car pr)) (lambda () (f (cdr pr))))))])
   (lambda () (f s))))

(define (stream-add-one-after s)
  (letrec ([f (lambda (s) (letrec ([pr (s)])
                            (cons (cons (car pr) 1) (lambda () (f (cdr pr))))))])
   (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (i) (cons (cons (list-nth-mod xs i) (list-nth-mod ys i))
                                (lambda () (f (+ i 1)))))])
   (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [f (lambda (i) (if (= i l)
                              #f
                              (letrec ([pr (vector-ref vec i)])
                                (if (pair? pr)
                                    (if (equal? v (car pr))
                                        pr
                                        (f (+ i 1))
                                     )
                                    (f (+ i 1))
                                 )
                               )
                           ))])
    (f 0)))

(define (caching-assoc xs n)
  (letrec ([cache (build-vector n (lambda (x) #f))]
           [pos 0]
           )
    (lambda (v)
      (letrec ([pr (vector-assoc v cache)])
          (if pr
              pr
              (letrec ([ret (assoc v xs)])
                (if ret
                    (begin
                      (vector-set! cache pos ret)
                      (set! pos (remainder (+ pos 1) n))
                      ret
                     )
                    #f
                    )))))))

(define-syntax while-greater
  (syntax-rules (do)
    [(while-greater e1 do e2)
     (letrec ([v1 e1]
              [f (lambda () (if (<= e2 v1) #t (f)))])
       (f)
       )]))