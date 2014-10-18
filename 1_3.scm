; P 1.29 Simpsons Rule

(define (simpsons F a b n)
		(define h (/ (- b a) n))
		(define (even y) (+ y (* 2 h)))
            	 (define (summation x Next sum) 
				(if (> x b) sum (summation (Next x) Next (+ (F x) sum))))
			(* (/ h 3) (+ (F a) (F b) (* 2 (summation (even a) even 0)) (* 4 (summation (+ a h) even 0)) )))


;; P 1.30
(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) (sum term (next a) next b))))

(define (sum-iter term a next b)
	(define (iter a result)
		(if (> a b) result (iter (next a) (+ result (term a)))))
	(iter a 0))

; P 1.31
(define (product term a next b)
		(define (iter a result)(if (> a b) result (iter (next a) (* result (term a)))))
	(iter a 1))

(define (fact a)
	(product (lambda (x) x) 1 (lambda (x) (+ x 1)) a))

(define (pi-by-4) 
		(define (term a) (* a a))
		(define (next a) (+ a 2))
		(define (limit) 1000)
	(/ (* 2 (product term 4.0 next limit)) (product term 3.0 next limit)))

; P 1.31 b
(define (product-r term a next b)
                (if (> a b) 1 (* (term a) (product-r term (next a) next b))))

; P 1.32

(define (accumulate combiner null-value term a next b) 
		(define (iter a result) (if (> a b) result (iter (next a) (combiner result (term a)))))
	(iter a null-value))

(define (sum-a term a next b) 
	(accumulate + 0 term a next b))

(define (product-a term a next b)
        (accumulate * 1 term a next b))

(define (accumulate-r combiner null-value term a next b)
		(if (<= a b) (combiner (term a) (accumulate-r combiner null-value term (next a) a next b)) (null-value))
	(accumulate-r combiner null-value term a next b))

;P 1.33 left

; p 1.35 Divide phi both the size

; P 1.36

(define tolerance 0.00001)
(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance))
	(define (try guess)
		(display guess)
		(newline)
		(let ((next (f guess)))
			(if (close-enough? guess next)
			next
			(try next))))
		(try first-guess))

(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2)

; P 1.37


(define (cont-frac n d k)
		(if (= k 0) 
			0 
			(let 
				((numerator (n k)) 
				(denominator (d k)))
			(/ numerator (+ denominator (cont-frac n d (- k 1)))))))

 
