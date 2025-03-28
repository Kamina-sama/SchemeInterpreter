(define null (list))
(define nil null)
(define nil? (lambda (x) (equal? x (list))))
(define null? nil?)

(define map
  (lambda (transformer ls)
    (if (nil? ls)
        nil
        (cons (transformer (car ls)) (map transformer (cdr ls))))))

(define filter 
  (lambda (proc ls) 
    (if (nil? ls) 
        nil
        (begin (define head (car ls))
               (define rest (cdr ls))
               (define head-is-part-of-filtered-list (proc head))
               (define filter-add-head (lambda (ls) (if head-is-part-of-filtered-list (cons head ls) ls)))
               (filter-add-head (filter proc rest))))))

(define foldl 
  (lambda (transformer acc ls)
    (if (nil? ls)
        acc
        (foldl transformer 
               (transformer (car ls) acc) 
               (cdr ls)))))

(define add1 (lambda (n) (+ n 1)))
(define sub1 (lambda (n) (- n 1)))
(define zero? (lambda (n) (= n 0))) 
(define positive? (lambda (n) (< 0 n)))
(define negative? (lambda (n) (< n 0)))
(define even? (lambda (n) (zero? (modulo n 2))))
(define odd? (lambda (n) (not (even? n))))
(define != (lambda (n1 n2) (not (= n1 n2))))
(define sqrt (lambda (n) (expt n 0.5)))

(define first car)
(define second (lambda (ls) (car (cdr ls))))
(define third (lambda (ls) (car (cdr (cdr ls)))))
(define fourth (lambda (ls) (car (cdr (cdr (cdr ls))))))
(define rest cdr)