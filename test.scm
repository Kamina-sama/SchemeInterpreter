(define fact-rec
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (sub1 n))))))

(define fact-almost
  (lambda (h n)
    (if (zero? n)
        1
        (* n (h h (sub1 n))))))

(define fact-rec2 (lambda (n) (fact-almost fact-almost n)))

(define fact 
  (lambda (n) 
    (define fact-iter 
      (lambda (n acc)
        (if (zero? n) 
            acc
            (fact-iter (sub1 n) (* n acc)))))
    (fact-iter n 1)))

(define make-countdown
  (lambda (timer)
    (lambda ()
      (if (zero? timer)
          timer
          (begin (set! timer (sub1 timer))
                 (add1 timer))))))
