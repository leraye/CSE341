#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct isgreater (e1 e2)    #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3) #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair   (e1 e2) #:transparent) ;; make a new pair
(struct first   (e)     #:transparent) ;; get first part of a pair
(struct second  (e)     #:transparent) ;; get second part of a pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist rlist)
  (if (null? rlist)
      (munit)
      (apair (car rlist) (racketlist->mupllist (cdr rlist)))))

(define (mupllist->racketlist mlist)
  (if (munit? mlist)
      null
      (cons (apair-e1 mlist) (mupllist->racketlist (apair-e2 mlist)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(isgreater? e)
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (int 1)
                   (int 0))
               (error "MUPL isgreater applied to non-number")))]
        [(ifnz? e)
         (let ([v (eval-under-env (ifnz-e1 e) env)])
           (if (int? v)
               (if (equal? (int-num v) 0)
                   (eval-under-env (ifnz-e3 e) env)
                   (eval-under-env (ifnz-e2 e) env))
               (error "MUPL ifnz applied to non-number")))]
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(first? e)
         (let ([pr (eval-under-env (first-e e) env)])
           (if (apair? pr)
             (apair-e1 pr)
             (error "MUPL first applied to non-apair")))]
        [(second? e)
         (let ([pr (eval-under-env (second-e e) env)])
           (if (apair? pr)
             (apair-e2 pr)
             (error "MUPL second applied to non-apair")))]
        [(int? e) e]
        [(fun? e) (closure env e)]
        [(mlet? e)
         (let ([arg (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) arg) env)))]
        [(call? e)
         (let ([f (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? f)
               (let* ([fc (closure-fun f)]
                      [fname (fun-nameopt fc)]
                      [new_env (cons (cons (fun-formal fc) arg) (closure-env f))])
                 (if (null? fname)
                     (eval-under-env (fun-body fc) new_env)
                     (eval-under-env (fun-body fc) (cons (cons fname f) new_env)))
                 )
               (error "Wrong argument passed")))]
        [(munit? e) e]
        [(ismunit? e) (let ([arg (eval-under-env (ismunit-e e) env)])
                        (if (munit? arg)
                            (int 1)
                            (int 0)))]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3)
  (ifnz (ismunit e1) e2 e3))

(define (mlet* bs e2)
  (if (null? bs)
      e2
      (let ([pr (car bs)])
        (mlet (car pr) (cdr pr) (mlet* (cdr bs) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list ("_x" e1) ("_y" e2))
         (ifnz (isgreater (var "_x") (var "_y")) e4 (ifnz (isgreater (var "_y") (var "_x")) e4 e3))
   ))

;; Problem 4

(define mupl-filter
  (fun null "f"
       (fun "g" "xs"
            (ifmunit (var "xs")
                     (munit)
                     (ifnz (call (var "f") (first (var "xs")))
                           (apair (first (var "xs")) (call (var "g") (second (var "xs"))))
                           (call (var "g") (second (var "xs"))))))))

(define mupl-all-gt
  (mlet "filter" mupl-filter
        (fun null "x"
             (call (var "filter") (fun null "y" (isgreater (var "y") (var "x")))))))
     

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (struct res (e fvs)) ; result type of f (could also use a pair)
    (define (f e) 
      (cond [(var? e) (res e (set (var-string e)))]
            [(int? e) (res e (set))]
            [(add? e) (let ([r1 (f (add-e1 e))]
                            [r2 (f (add-e2 e))])
                        (res (add (res-e r1) (res-e r2))
                             (set-union (res-fvs r1) (res-fvs r2))))]
            [(isgreater? e) (let ([r1 (f (isgreater-e1 e))]
                                  [r2 (f (isgreater-e2 e))])
                              (res (isgreater (res-e r1) (res-e r2))
                                  (set-union (res-fvs r1) (res-fvs r2))))]
            [(ifnz? e) (let ([r1 (f (ifnz-e1 e))]
                             [r2 (f (ifnz-e2 e))]
                             [r3 (f (ifnz-e3 e))])
                         (res (ifnz (res-e r1) (res-e r2) (res-e r3))
                              (set-union (res-fvs r1) (res-fvs r2) (res-fvs r3))))]
            [(fun? e) (let* ([r (f (fun-body e))]
                             [fvs (set-remove (res-fvs r) (fun-formal e))]
                             [fvs (if (fun-nameopt e) 
                                      (set-remove fvs (fun-nameopt e)) 
                                      fvs)])
                        (res (fun-challenge (fun-nameopt e) (fun-formal e) 
                                            (res-e r) fvs)
                            fvs))]
            [(call? e) (let ([r1 (f (call-funexp e))]
                             [r2 (f (call-actual e))])
                        (res (call (res-e r1) (res-e r2))
                             (set-union (res-fvs r1) (res-fvs r2))))]
            [(mlet? e) (let* ([r1 (f (mlet-e e))]
                              [r2 (f (mlet-body e))])
                         (res (mlet (mlet-var e) (res-e r1) (res-e r2))
                              (set-union (res-fvs r1) (set-remove (res-fvs r2)   (mlet-var e)))))]
            [(apair? e) (let ([r1 (f (apair-e1 e))]
                              [r2 (f (apair-e2 e))])
                          (res (apair (res-e r1) (res-e r2))
                             (set-union (res-fvs r1) (res-fvs r2))))]
            [(first? e) (let ([r (f (first-e e))])
                        (res (first (res-e r))
                             (res-fvs r)))]
            [(second? e) (let ([r (f (second-e e))])
                        (res (second (res-e r))
                             (res-fvs r)))]
            [(munit? e) (res e (set))]
            [(ismunit? e) (let ([r (f (ismunit-e e))])
                            (res (ismunit (res-e r))
                                 (res-fvs r)))]))
    (res-e (f e)))

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) 
  (cond [(fun-challenge? e)
         (closure (set-map (fun-challenge-freevars e)
                           (lambda (s) (cons s (envlookup env s))))
                  e)]
        [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) e]
        [(closure? e) e]
        [(munit? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(isgreater? e)
         (let ([v1 (eval-under-env-c (isgreater-e1 e) env)]
               [v2 (eval-under-env-c (isgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (int 1)
                   (int 0))
               (error "MUPL isgreater applied to non-number")))]
        [(ifnz? e)
         (let ([v (eval-under-env-c (ifnz-e1 e) env)])
           (if (int? v)
               (if (equal? (int-num v) 0)
                   (eval-under-env-c (ifnz-e3 e) env)
                   (eval-under-env-c (ifnz-e2 e) env))
               (error "MUPL ifnz applied to non-number")))]
        [(apair? e) (apair (eval-under-env-c (apair-e1 e) env) (eval-under-env-c (apair-e2 e) env))]
        [(first? e)
         (let ([pr (eval-under-env-c (first-e e) env)])
           (if (apair? pr)
             (apair-e1 pr)
             (error "MUPL first applied to non-apair")))]
        [(second? e)
         (let ([pr (eval-under-env-c (second-e e) env)])
           (if (apair? pr)
             (apair-e2 pr)
             (error "MUPL second applied to non-apair")))]
        [(mlet? e)
         (let ([arg (eval-under-env-c (mlet-e e) env)])
           (eval-under-env-c (mlet-body e) (cons (cons (mlet-var e) arg) env)))]
        [(ismunit? e) (let ([arg (eval-under-env-c (ismunit-e e) env)])
                        (if (munit? arg)
                            (int 1)
                            (int 0)))]
        [(call? e)
         (let ([f (eval-under-env-c (call-funexp e) env)]
               [arg (eval-under-env-c (call-actual e) env)])
           (if (closure? f)
               (let* ([fc (closure-fun f)]
                      [fname (fun-challenge-nameopt fc)]
                      [new_env (cons (cons (fun-challenge-formal fc) arg) (closure-env f))])
                 (if (null? fname)
                     (eval-under-env (fun-challenge-body fc) new_env)
                     (eval-under-env (fun-challenge-body fc) (cons (cons fname f) new_env)))
                 )
               (error "Wrong argument passed")))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
