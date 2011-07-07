(define (null? ls) (eqv? ls '()))

(define (list . args) args)

(define (foldr f acc ls) (if (null? ls) acc (f (car ls) (foldr f acc (cdr ls)))))

(define (foldl f acc ls) (if (null? ls) acc (foldl f (f acc (car ls)) (cdr ls))))

(define (map f ls) (foldr (lambda (x ms) (cons (f x) ms)) '() ls))

(define (filter p ls)
  (foldr (lambda (x ms)
          (if (p x)
            (cons x ms)
            ms)) '() ls))

(define (not b) (if b #f #t))

(define (id x) x)

(define (flip f) (lambda (x y) (f y x)))

(define (compose f g) (lambda (x) (f (apply g x))))

(define zero? (= 0))
(define positive? (< 0))
(define negative? (> 0))
(define (even? n) (= (mod n 2) 0))
(define (odd? n)  (= (mod n 2) 1))

(define fold foldl)
(define reduce fold)

(define (unfold gen seed pred)
  (if (pred seed)
    (cons init '())
    (cons init (unfold gen (gen init) pred))))

(define (sum . lst)     (fold + 0 lst))
(define (product . lst) (fold * 1 lst))
(define (and . lst)     (fold && #t lst))
(define (or . lst)      (fold || #f lst))

(define (fact n)
  (define (fA n a)
    (if (= 1 n)
      a
      (fA (- n 1) (* n a))))
  (fA n 1))


(define (fib n)
  (define (fA n n2 n1)
    (if (= 0 n)
      n1
      (if (= 1 n)
        n2
        (fA (- n 1) (+ n1 n2) n2))))
  (fA n 1 0))

(define (max first . rest)
  (fold (lambda (m n)
          (if (> m n) m n)) first rest))

(define (min first . rest)
  (fold (lambda (m n)
          (if (< m n) m n)) first rest))

(define (length l)
  (fold (lambda (a x) (+ a 1)) 0 l))

(define (reverse l)
  (fold (flip cons) '() l))

(define (mem-helper p f)
  (lambda (a n)
    (if (and (not a) (p (f n)))
      n
      a)))

(define (memq i ls) (fold (mem-helper (eq? i) id) #f ls))
(define (memv i ls) (fold (mem-helper (eqv? i) id) #f ls))
(define (member i ls) (fold (mem-helper (equal? i) id) #f ls))

(define (assq i ls) (fold (mem-helper (eq? i) car) #f ls))
(define (assv i ls) (fold (mem-helper (eqv? i) car) #f ls))
(define (assoc i ls) (fold (mem-helper (equal? i) car) #f ls))

(define (cadr ls) (car (cdr ls)))
(define (caar ls) (car (car ls)))
(define (pair x y) (cons x (cons y '())))

(define (partition p ls)
  (define (add-in n ps) (pair (cons n (car ps)) (cadr ps)))
  (define (add-out n ps) (pair (car ps) (cons n (cadr ps))))
  (foldr (lambda (x parts)
    (if (p x)
      (add-in x parts)
      (add-out x parts))) '(() ()) ls))
