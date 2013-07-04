#lang racket

;;载入辅助过程及常量
(require "sicp_utils.rkt")

;--------------------------------------------------------------
;1.8
;--------------------------------------------------------------
;;1.8 求立方根
(define (cube-root guess next-guess x)
  ;;两个数是否足够近
  (define (good-enough? v1 v2)
    (< (/ (abs (- v1 v2)) v2) tolerance)) 
  ;;改进猜测
  (define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  
   (if (good-enough? guess next-guess)
      guess
      (cube-root next-guess (improve next-guess x) 
                 x)))
;--------------------------------------------------------------
;1.16~1.18
;--------------------------------------------------------------
;;1.16 快速幂运算 b^n
(define (fast-expt b n)
  (define (fast-expt-iter result c count)
    (cond ((= count 0) result)
          ((even? count)
           (fast-expt-iter result (square c) (halve count)))
          (else (fast-expt-iter  (* result c) c (dec count)))))
  (fast-expt-iter 1 b n))

;;1.18 快速乘法运算 a*b
(define (fast-mul a b)
  (define (fast-mul-iter result x n)
    (cond ((= n 0) result)
          ((even? n)
           (fast-mul-iter result (double x) (halve n)))
          (else (fast-mul-iter (+ result x) x (- n 1)))))
  (fast-mul-iter 0 a b))
;--------------------------------------------------------------
;1.22~1.28
;--------------------------------------------------------------
;;找出n的最小因子
(define (smallest-divisor n)
  ;;1.23(修改版)
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? n test-divisor) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

;;判断是否是素数   
(define (prime? n)
  (if (not (= n 1))
      (= n (smallest-divisor n))
      false))

;;1.22(测试)
(define (start-prime-test n start-time)
  (and (prime? n)
       (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (display n)
  (display "  ***  ")
  (display elapsed-time)
  (newline))
          
(define (search-for-primes start n)
  (define (search-for-primes-iter start count start-time)
    (if (= count 0)
        (display "****************************")
        (search-for-primes-iter (+ start 2) 
                                (if(start-prime-test start start-time) (dec count) count)
                                (runtime)))) 
 (search-for-primes-iter (if (even? start) (+ 1 start) start)
                         n 
                         (runtime)))
;;测试启动函数
(define (search-for-primes-test)
  (search-for-primes 100000000 3)
  (newline)
  (search-for-primes 1000000000 3)
  (newline)
  (search-for-primes 10000000000 3)
  (newline)
  (search-for-primes 100000000000 3))

;;1.24 费马检测
(define (fermat-test n)
  (define (expmod base exp m)
    (remainder (fast-expt base exp) m))
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fermat-fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fermat-fast-prime? n (- times 1)))
        (else false)))  

;;1.28 miller-rabin检测
;检查a是不是1取模n的非平凡平方根
(define (check-notrivial-sqrt-of-one a n)
  (define t (remainder (square a) n))
  (if (and (> a 1) (< a (- n 1)) (= t 1))
      0
      t))

(define (miller-rabin-test n)
  (define (fast-expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (check-notrivial-sqrt-of-one (fast-expmod base (halve exp) m) m))
          (else (remainder (* base (fast-expmod base (dec exp) m)) m))))
  (define (try-it a)
    (= (fast-expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (miller-rabin-fast-prime? n (- times 1)))
        (else false))) 
;--------------------------------------------------------------
;1.29~1.33
;--------------------------------------------------------------
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;辛普森规则求积分
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (* (f (+ a (* k h)))
       (if (even? k) 2 4)))  
  (* (/ h 3)
     (+ (f a)
        (f (+ a (* n h)))
        (sum y 1 inc (- n 1)))))
 
;;1.30通用叠加器
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
;;1.31通用叠乘器
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;根据公式计算π
(define (calc-pi n)
  (define (y k)
    (square (- 1 (/ 1 (+ (* 2 k) 1)))))
  (* (product-iter y 1 inc (/ n 2)) (* 2.0 n))) 

;;1.32通用叠代计算器
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        null-value
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate-sum term a next b)
  (accumulate + 0 term a next b))

(define (accumulate-product term a next b)
  (accumulate * 1 term a next b))

;;1.33 带过滤器
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a b)
                    (term a)
                    null-value)
                (filtered-accumulate filter combiner null-value term (next a) next b))))
(define (coprime? x n)
  (if (= n 1)
      false
      (= (gcd x n) 1)))
;--------------------------------------------------------------
;1.36~1.39
;--------------------------------------------------------------
;;1.36 不动点计算 显示中间过程
(define (fixed-point f first-guess)
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;;1.37
;(define (cont-frac n d k)
;  (define (cont-frac-iter result i)
;    (if (= i 1)
;        result
;        (cont-frac-iter (+ (d (- i 1)) 
;                           (/ (n i) result)) (- i 1))))
;  (cont-frac-iter (+ (d (- k 1)) (/ (n k) (d k))) k))

(define (cont-frac n d k)
  (define (cont-frac-recur i)
    (if (= i k)
        (+ (d (- k 1)) (/ (n k) (d k)))
        (/ (n i) (+ (d i) (cont-frac-recur (+ i 1))))))
  (cont-frac-recur 1))
;;1.38
(define (d x)
  (let ((temp (remainder x 3)))
       (if (or (= temp 1) (= temp 0))
            1
            (* 2 (+ (floor (/ x 3)) 1)))))

(define (calc-e k)
  (+ 2 (cont-frac (lambda (x) 1.0) d k)))
;;1.39
(define (tan-cf x k)  ;;x/tanx -> f(k)=(2k-1) - x^2/f(k+1)
  (define (tan-cf-recur i)
    (if (= i k)
        (- (* 2.0 k) 1)
        (- (- (* 2 i) 1)
           (/ (square x) (tan-cf-recur (+ i 1))))))
  (/ x (tan-cf-recur 1)))
;--------------------------------------------------------------
;1.40~1.46
;--------------------------------------------------------------
;;1.40
(define (cubic a b c)
    (lambda (x)
        (+ (cube x) (* a (square x)) (* b x) c)))
;;1.41
(define (double-func f)
  (lambda (x) ( f (f x))))
;;1.42
(define (compose f g)
  (lambda (x) (f (g x))))
;;1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))
;;1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) 
                    (f x) (f (+ x dx))) 
                 3)))

;1.45
;使用repeated求幂运算
(define (expt-repeated base n)
  (if (= n 0)
      1
      ((repeated (lambda (x) (* base x)) n) 1)))

;;求一次平均阻尼
(define (average-damp f)
  (lambda (x) (average x (f x))))

;;对函数f求n次平均阻尼
(define (average-damp-n-times n f)
  ((repeated average-damp n) f))
;;求k次方根需要n次平均阻尼
(define (damped-n-root n damp-times)
  (lambda (x) (fixed-point (average-damp-n-times 
                            damp-times
                            (lambda (y) (/ x (expt-repeated y (- n 1))))) 1.0))) 
;1.46
(define (iterative-improve f g)
  (define (inner-func guess)
    (let ((next (g guess)))
         (if (f next guess)
             guess
             (inner-func next))))
  inner-func) 

(define (sqrt-of-iter-impr x)
  (define (improve guess)  ;;改进猜测
    (average guess (/ x guess)))
  ((iterative-improve close-enough? improve) 1.0))

(define (fixed-point-of-iter-impr func first-guess)
  (define (improve guess);;改进猜测
    (func guess))
  ((iterative-improve close-enough? improve) first-guess))