

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (sub1 n))))))

(fact 5)

((lambda (n)
   ((lambda (fact)
      ((fact fact) n))
    (lambda (fact)
      (lambda (n)
        (if (zero? n)
            1
            (* n ((fact fact) (sub1 n)))))))) 5)
