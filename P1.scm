(define (abs x) (cond
                    ((< x 0) (- x))
                    (else x)))
(abs 21)
(abs -23)
(abs 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; P 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P 1.5 is brilliant

(define (p) (p))
(define (test x y)
    (if (= x 0) 0 y))

(test 0 (p))
;; Behavior when the substituion model is evaluated as a Applicative-order evaluation
; The second-argument of (test 0 (p)) is evaluated before `test` is expanded. So it attemps to evaluate (p)
;(p) ---(evaluates to)--> (p) --> (p) --> (p) ..... running in an iterative loop by self-recursive process.
; The program never terminates. Neither will it complain of no more space available

;; Behavior if evaluated by Normal-order
(test 0 (p)) ; evaluates to ->
(if (= 0 0) 0 (p)) ;(p) is again not evaluated.
0

; Had it been
(test 1 (p))
(if (= 1 0) 0 (p)) ;(p) is not evaluated.
(p) ; The alternative block of `if` is not executed and it runs in an infinite self-recursive iterative loop

;Not a precise explanation. But the behavior can be visualized by comparing it to
((test 1 p)) ; Where the second argument is not evaluated till explicitly called in the end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; P 1.6
(define (if-new predicate a b)
    (cond (predicate a) (else b)))

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))


(define (sqrt-iter guess x)
    (if-new (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x)
                       x)))

; new-if` is a procedure. `if` is a special-form.
; In (if <predicate> <consequent> <alternative>) . `consequent` is only evaluated if `predicate` evaluates to `true`.
;                                                                       `alternative` is only evaluated if `predicate` evaluates to `false`. Only either one is evaluated
;   For example:
(define (p) (p))
(if (= 0 0) 0 (p)) ; This evaluates to 0.

; In case of 'if-new`
(if-new (= 0 0) 0 (p))
;          If Runtime evalautes procedure in `Applicative-Order` then: the expression evaluates to: infinite iterative loop due to (p)
;                                         in `Normal-order` then: it evaluates to 0
;
;; Food-For-Thought: Can we say that for a runtime which evaluates in `Normal-Order`, is `if-new` same as `if`?
; Proof by Contradiction: Hypothesis: A runtime which evaluates in `Normal-Order`, `(if-new p a b)` is not same as `(if p a b)`
;               where p,a,b are deterministic expressions and pure procedures. (i.e. when evaluated they return the same value always)
;
; Then ∃(p a b) such that (if-new p a b) => x1 and (if p a b) => x2. And x1 != x2 (=> represents evaluation)
;
; a' represents that a has been evaluated.
;
; Case-1: p whenever evaluated, returns true
;   (if-new p a b) => (cond (p a)(else b)) => (cond (p' a)(else b)) => a'
;   (if p a b) => p' => a'
;
; Case-2: p whenever evaluated, returns false
;   (if-new p a b) => (cond (p a)(else b)) => (cond (p' a)(else b)) => b'
;   (if p a b) => p' => b'
;
; In each of the case, both the expressions provide the same result. This is a contradiction as our assumption suggests they are different. Hence (if p a b) can be replaced with (if-new p a b)

;; In the case of `sqrt-iter` above:
; If the runtime evaluates in `Application-Order`:
;                   then the `sqrt-iter` above results in an infinite loop. As the `alternative` in `if-new` block is executed before even `cond` procedure is evaluated. And this happens for every call
;; If the runtime evaluates in `Normal-Order`:
;                   then the `sqrt-iter` above gives the same result as it would with `if` block.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ;;;;;;;;;;;;;;;;;;;

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
    (define (product p num) (if (> num a) p (product (* p num) (+ num 1))))
    (product 1 1))

; P 1.10

(define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A
                    (- x 1)
                    (A x (- y 1))))))

; Mathematically.
; a) A (0,y) = 2 * y
;
; b) If y >= 2 then A(1, y) = A(1-1, A (1, y-1)) = A(0, A(1, y-1)) = 2 * A(1, y-1)
;   So: A(1,y) = if y =0 , 0
;                if y >=1 , 2 ^ 2 y
;
; c) If y >= 1 then A(2,y) = A(1, A(2, y-1))
;                          = 2 ^ (A(2, y-1)) ;; From (b). If  (y-1) >= 1 then,
;                          = 2 ^ (2 ^ (A(2, y-2))) ;; Obtained by applying above.
;   So: A(2,y) = if y = 0, 0
;                if y >=1, (2 ^ (2 ^ (2 ^ ... y-times)))
(A 1 10) ; From (b) : Math.pow(2, 10) = 1024
(A 2 4) ;; From (c) : 2 ^(2 ^(2 ^ 2)) = 65536

So:
(define (f n) (A 0 n)) = 2 * y (from (a))
(define (g n) (A 1 n)) = 0, if n =0
                         2 ^ n, if n>=1 (from (b))
(define (h n) (A 2 n)) = 0, if n =0
                         (2 ^ (2 ^ (2 ^ .... n times))) (from (c))
(define (k n) (* 5 n n)) = 5*n*n


;P 1.11
(define (f n) (if (< n 3)
                  n
                  (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
