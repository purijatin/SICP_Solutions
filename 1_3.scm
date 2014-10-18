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
	(define (recur num)
		(if (> num k) 
			0 
			(let 
				((numerator (n num)) 
				(denominator (d num)))
			(/ numerator (+ denominator (recur (+ num 1)))))))
	(recur 1))

(define (cont-frac-r n d k)
		(define (iter ans num) 
			(if (= num 0) 
				ans
				   (let
					 ((numerator (n num))
        	                         (denominator (d num)))
					(iter (/ numerator (+ ans denominator)) (- num 1)))))
		(iter (/ (n k) (d k))  (- k 1))) 

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100)
(cont-frac-r (lambda (i) 1.0) (lambda (i) 1.0) 100)

(define phi-inv (/ 1 (/ (+ 1 (sqrt 5)) 2)))

(define (k-phi? func n d ans apprx) 
		(define (iter k) 
			(display (func n d k))
			(newline)
			(if (< (abs (- (func n d k) ans)) apprx) k (iter (+ k 1))))
		(iter 1))

(k-phi? cont-frac (lambda (i) 1.0) (lambda (i) 1.0) phi-inv 0.0001)
(k-phi? cont-frac-r (lambda (i) 1.0) (lambda (i) 1.0) phi-inv 0.0001)


; P 1.38
(define e (+ 2 (cont-frac (lambda (i) 1.0) 
		     (lambda (i) 
				(if (or (= (remainder i 3) 0) (= (remainder i 3) 1 )) 
					1 
					(+ 2 (* (floor (/ i 3)) 2))))
			10)))

(define (e-general func k)
		(+ 2 (func (lambda (i) 1.0)
                     (lambda (i) 
                                (if (or (= (remainder i 3) 0) (= (remainder i 3) 1 ))
                                        1 
                                        (+ 2 (* (floor (/ i 3)) 2))))
                        k)))

;; Automatically finds k based on error allowed
(define (e-gen func approx)
		(define (n i) 1.0)
		(define (d i) (if (or (= (remainder i 3) 0) (= (remainder i 3) 1 ))
                                       1
                                      (+ 2 (* (floor (/ i 3)) 2))))
		(+ 2 (func n d (k-phi? func n d 0.718281828459045 approx))))

(e-general cont-frac-r 100)
(e-general cont-frac 100)
(e-gen cont-frac 0.00001)
(e-gen-r cont-frac 0.00001)

;P 1.40
(define (cubic a b c) (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

; P 1.41 depends applicative or normal
(define (double f) (lambda (x) (f (f x))))

; Applicative
((double (double double)) f)
=> ((d d (d d)) f)
=> ((d d (d (d f))))
=> ((d d (d   (f  (f)))))
=> ((d d      (f(f  (f(f))))))
=> ((d        (f(f(f(f  (f(f(f(f))))))))))
=> (f(f(f(f(f(f(f(f(   f(f(f(f(f(f(f(f))))))))))))))))

;Normal

; P 1.42
(define (compose f g) (lambda (x) (f (g x))))

; P 1.43
(define (repeated f x) (if (= 1 x) f (repeated (compose f f) (- x 1))))

