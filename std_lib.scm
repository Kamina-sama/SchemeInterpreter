(define null '())
(define nil '())
(define (nil? x) (equal? x '()))
(define (null? x) (equal? x '()))

(define (map proc ls)
  (if (nil? ls)
      nil
      (cons (proc (car ls))
            (map proc (cdr ls)))))

(define (filter proc ls)
  (cond [(nil? ls) nil]
        [else
         (define head (car ls))
         (define rest (cdr ls))
         (define head-is-part-of-filtered-list (proc head))
         (define (filter-add-head ls) (if head-is-part-of-filtered-list (cons head ls) ls))
         (filter-add-head (filter proc rest))]))

(define (foldl proc acc ls)
  (if (nil? ls)
      acc
      (foldl proc
             (proc (car ls) acc)
             (cdr ls))))

(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))
(define (zero? n) (= n 0))
(define (positive? n) (< 0 n))
(define (negative? n) (< n 0))
(define (even? n) (zero? (modulo n 2)))
(define (odd? n) (not (even? n)))
(define (!= n1 n2) (not (= n1 n2)))
(define (sqrt n) (expt n 0.5))

(define first car)
(define (second ls) (car (cdr ls)))
(define (third ls) (car (cdr (cdr ls))))
(define (fourth ls) (car (cdr (cdr (cdr ls)))))
(define (rest ls) (cdr ls))
(define (caar ls) (car (car ls)))
(define (cadr ls) (car (cdr ls)))
(define (cddr ls) (cdr (cdr ls)))
(define (cdar ls) (cdr (car ls)))

(define (member elem ls)
  (cond [(null? ls) #f]
        [(equal? elem (car ls)) ls]
        [else (member elem (cdr ls))]))

(define (assoc key dict)
  (cond [(null? dict) #f]
        [(equal? (caar dict) key) (car dict)]
        [else (assoc key (cdr dict))]))