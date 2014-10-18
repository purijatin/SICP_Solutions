(define (abs x) (cond
			((< x 0) (- x))	
			(else x)))
(abs 21)
(abs -23)
(abs 0)

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; Problem 1.3
(define (greater a b) (if (>= a b) a b))
(define (lesser a b) (if (= (greater a b) a) b a))
(define (greater-t a b c) (if (>= a b)
                                (greater a c)
	                        (greater b c)))
(define (second a b c) (if (>= a b)
                                (if (>= a c) (greater b c) a)
                                (if (>= a c) a (lesser b c))))

(define (p1-3 a b c) 
		(define (greater a b) (if (>= a b) a b))
		(define (lesser a b) (if (= (greater a b) a) b a))
		(define (greater-t a b c) (if (>= a b)
		                                (greater a c)
		                                (greater b c)))
		(define (second a b c) (if (>= a b)
                		                (if (>= a c) (greater b c) a)
		                                (if (>= a c) a (lesser b c))))

		(+ (* (greater-t a b c) (greater-t a b c))
			(* (second a b c) (second a b c))))
(p1-3 2 3 4)
;;;;;;;;;;;;;;

; P 1.5 is brilliant
; P 1.6
(define (if-new predicate a b) 
		(cond (predicate a)(else b)))

(define (sqrt x)
	
	(define (good-enough? guess x)
		(< (abs (- (square guess) x)) 0.001))
	(define (improve guess x)
		(average guess (/ x guess)))
	(define (sqrt-iter guess x)
		(if (good-enough? guess x)
			guess
			(sqrt-iter (improve guess x) x)))
	(sqrt-iter 1.0 x))

(define (factorial a) (* a (if (>= a 2) (factorial (- a 1)) 1)))
(define (fact a)
		(define (product p num)(if (> num a) p (product (* p num) (+ num 1))))
		(product 1 1))

;P 1.11
(define (f n) (if (< n 3)
		n
		(+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
