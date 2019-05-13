;; A simple CPS transformer which does proper tail-call and does not
;; duplicate contexts for if-expressions.

;; author: Yin Wang (yw21@cs.indiana.edu)


;; (load "pmatch.ss")     ;; substitute match to match
(import (tspl match))


(define cps
  (lambda (exp)
    (letrec
        ([trivial?  (lambda (x) (memq x '(zero? add1 sub1)))]
         [id (lambda (v) v)]
         [ctx0 (lambda (v) `(k ,v))]      ; tail context
         [fv (let ((n -1))
               (lambda ()
                 (set! n (+ 1 n))
                 (string->symbol (string-append "v" (number->string n)))))]
         [cps1
          (lambda (exp ctx)
            (match exp
              [,x (guard (not (pair? x))) (ctx x)]
              [(if ,test ,conseq ,alt)
               (cps1 test
                     (lambda (t)
                       (if (memq ctx (list ctx0 id))
                           `(if ,t ,(cps1 conseq ctx) ,(cps1 alt ctx))
                           (let ((v* (fv)))
                             `(let ((k (lambda (,v*) ,(ctx v*))))
                                (if ,t ,(cps1 conseq ctx0) ,(cps1 alt ctx0)))))))]
              [(lambda (,x) ,body)
               (ctx `(lambda (,x k) ,(cps1 body ctx0)))]
              [(,op ,a ,b)
               (cps1 a (lambda (v1)
                         (cps1 b (lambda (v2)
                                   (ctx `(,op ,v1 ,v2))))))]
              [(,rator ,rand)
               (cps1 rator
                     (lambda (r)
                       (cps1 rand
                             (lambda (d)
                               (cond
                                [(trivial? r) (ctx `(,r ,d))]
                                [(eq? ctx ctx0) `(,r ,d k)] ; tail call
                                [else
                                 (let ((v* (fv)))
                                   `(,r ,d (lambda (,v*) ,(ctx v*))))])))))]))])
      (cps1 exp id))))




;; tests

;; var
(cps 'x)                                ;x

(cps '(lambda (x) x))                   ;; (lambda (x k) (k x))

(cps '(lambda (x) (x 1)))               ;; (lambda (x k) (x 1 k))



;; no lambda (will generate identity functions to return to the toplevel)
(cps '(if (f x) a b))                   ;; (f x (lambda (v0) (if v0 a b)))

(cps '(if x (f a) b))                   ;; (if x (f a (lambda (v0) v0)) b)



;; if stand-alone (tail)
(cps '(lambda (x) (if (f x) a b)))
;; (lambda (x k) (f x (lambda (v0) (if v0 (k a) (k b)))))



;; if inside if-test (non-tail)
(cps '(lambda (x) (if (if x (f a) b) c d)))
;; (lambda (x k)
;;   (let ([k (lambda (v0) (if v0 (k c) (k d)))])
;;     (if x (f a k) (k b))))



;; both branches are trivial, should do some more optimizations
(cps '(lambda (x) (if (if x (zero? a) b) c d)))
;; (lambda (x k)
;;   (let ([k (lambda (v0) (if v0 (k c) (k d)))])
;;     (if x (k (zero? a)) (k b))))



;; if inside if-branch (tail)
(cps '(lambda (x) (if t (if x (f a) b) c)))


;; if inside if-branch, but again inside another if-test (non-tail)
(cps '(lambda (x) (if (if t (if x (f a) b) c) e w)))


;; if as operand (non-tail)
(cps '(lambda (x) (h (if x (f a) b))))


;; if as operator (non-tail)
(cps '(lambda (x) ((if x (f g) h) c)))


;; why we need more than two names
(cps '(((f a) (g b)) ((f c) (g d))))


;;; factorial
(cps
 '(lambda (n)
    ((lambda (fact)
       ((fact fact) n))
     (lambda (fact)
       (lambda (n)
         (if (zero? n)
             1
             (* n ((fact fact) (sub1 n)))))))))



;; factorial
((eval
  (cps
   '(lambda (n)
      ((lambda (fact)
         ((fact fact) n))
       (lambda (fact)
         (lambda (n)
           (if (zero? n)
               1
               (* n ((fact fact) (sub1 n))))))))))
 5
 (lambda (v) v))                        ;;; 120

(cps
 '(lambda (n)
    ((lambda (fib)
       ((fib fib) n))
     (lambda (fib)
       (lambda (n)
         (cond [(zero? n) 0]
               [(zero? (- n 1)) 1]
               [else
                (+  ((fib fib) (- n 1))
                    ((fib fib) (- n 2)))]))))))


(cps
 '(lambda (n)
    ((lambda (fib)
       ((fib fib) n))
     (lambda (fib)
       (lambda (n)
         (if (zero? n)
             0
             (if (zero? (- n 1))
                 1
                 (+  ((fib fib) (- n 1))
                     ((fib fib) (- n 2))))))))))

;; (lambda (n k)
;;   ((lambda (fib k) (fib fib (lambda (v0) (v0 n k))))
;;     (lambda (fib k)
;;       (k (lambda (n k)
;;            (if (zero? n)
;;                (k 0)
;;                (if (zero? (- n 1))
;;                    (k 1)
;;                    (fib fib
;;                         (lambda (v1)
;;                           (v1 (- n 1)
;;                               (lambda (v2)
;;                                 (fib fib
;;                                      (lambda (v3)
;;                                        (v3 (- n 2)
;;                                            (lambda (v4)
;;                                              (k (+ v2 v4)))))))))))))))
;;     k))



((eval
  (cps
   '(lambda (n)
      ((lambda (fib)
         ((fib fib) n))
       (lambda (fib)
         (lambda (n)
           (if (zero? n)
               0
               (if (zero? (- n 1))
                   1
                   (+  ((fib fib) (- n 1))
                       ((fib fib) (- n 2)))))))))))
 10
 (lambda (v) v))



;; cond not implemented yet
((eval
  (cps
   '(lambda (n)
      ((lambda (fib)
         ((fib fib) n))
       (lambda (fib)
         (lambda (n)
           (cond [(zero? n) 0]
                 [(zero? (- n 1)) 1]
                 [else
                  (+  ((fib fib) (- n 1))
                      ((fib fib) (- n 2)))])))))))
 5
 (lambda (v) v))
