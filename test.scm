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

(define (identity x) x)
(define (fact-k n k)
  (cond [(zero? n) (k 1)]
        [else
         (define (new-k z) (k (* n z)))
         (define new-n (sub1 n))
         (fact-k new-n new-k)]))

(define make-countdown
  (lambda (timer)
    (lambda ()
      (if (zero? timer)
          timer
          (begin (set! timer (sub1 timer))
                 (add1 timer))))))

(define (greet name) (string-append "Hi," " " name))
(define (e-greet name) (greet (string-upcase name)))

(define (to-infinity n) (to-infinity (add1 n)))
(define-macro (new-and . args)
  (car args))

(define-macro (and a1 a2)
  `(if (not ,a1)
       #f
       (if (not ,a2)
           #f
           #t)))
