#lang racket

;;载入辅助过程及常量
(require "sicp_utils.rkt")

;--------------------------------------------------------------
;2.1
;--------------------------------------------------------------
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0) 
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))
;--------------------------------------------------------------
;2.2~2.3
;--------------------------------------------------------------
;;2.2
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (make-segment x1 y1 x2 y2)
  (cons (make-point x1 y1) (make-point x2 y2)))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (midpoint-segment segment)
  (make-point 
   (/ (+ (car (start-segment segment)) (car (end-segment segment))) 2.0)
   (/ (+ (cdr (start-segment segment)) (cdr (end-segment segment))) 2.0)))
;;2.3略
;--------------------------------------------------------------
;2.4~2.6
;--------------------------------------------------------------
;;2.5
(define (my-cons x y)
  (* (expt 2 x) (expt 3 y)))
(define (count-0-remainder-divisions n divisor)
  (define (iter exp)
    (if (= (remainder n (expt divisor exp)) 0)
        (iter (inc exp))
        (dec exp)))
  (iter 1))
(define (my-car z)
  (count-0-remainder-divisions z 2))
(define (my-cdr z)
  (count-0-remainder-divisions z 3))
;;2.6
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))
;--------------------------------------------------------------
;2.7~2.16
;--------------------------------------------------------------
;;2.7
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))
;;2.8
(define (make-interval a b) (cons a b))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
;;2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Divided By span-0 Area")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))
;;2.12
(define (make-center-percent x percent)
  (let ((deviation (* x percent)))
    (make-interval (- x deviation) (+ x deviation))))
(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2.0))
(define (percent x)
  (/ (- (upper-bound x) (center x)) (center x)))
;--------------------------------------------------------------
;2.17~2.23
;--------------------------------------------------------------
;;2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))
;;2.18
(define (reverse l)
  (define (iter l1 l2)
    (if (null? l1)
        l2
        (iter (cdr l1) (cons (car l1) l2))))
  (iter l null))
;;2.19
(define (except-first-denomination lst)
  (cdr lst))
(define (first-denomination lst)
  (car lst))
(define (no-more? lst)
  (null? lst))
;;2.20
(define (same-parity fst . oth)
  (define (same-parity-iter src)
    (if (null? src)
        null
        (if (even? (+ fst (car src)))
            (cons (car src) (same-parity-iter (cdr src)))
            (same-parity-iter (cdr src)))))
  (cons fst (same-parity-iter oth)))
;;2.23
(define (my-for-each proc lst)
  (if (null? lst)
      true
      (begin (proc (car lst))
             (my-for-each proc (cdr lst)))))
;--------------------------------------------------------------
;2.27~2.32
;--------------------------------------------------------------
;;2.27
(define (deep-reverse lst)
  (let ((reverse-lst (reverse lst)))
    (map (lambda (x)
           (if (list? x)
               (deep-reverse x)
               x))
         reverse-lst)))
;;2.28
(define (fringe-1 x)
  (define (iter tree lst)
    (cond ((null? tree) lst)
          ((not (pair? tree)) (cons tree lst))
          (else (iter (car tree) (iter (cdr tree) lst)))))
  (iter x null))

(define (fringe-2 tree) 
  (cond ((null? tree) null) 
        ((not (pair? tree)) (list tree)) 
        (else (append (fringe-2 (car tree)) (fringe-2 (cdr tree)))))) 

;;2.29
;(a)
(define (make-mobile left right) 
  (list left right)) 
(define (make-branch length structure) 
  (list length structure)) 

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

;; A test mobile: 
;; Level 
;; ----- 
;; 3                   4  |    8                                      
;;              +---------+--------+ 2                        
;; 2         3  |  9                                        
;;        +-----+----+ 1                                    
;; 1    1 | 2                                       
;;    +---+---+                             
;;    2       1                             

(define level-1-mobile (make-mobile (make-branch 2 1) 
                                    (make-branch 1 2))) 
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                    (make-branch 9 1))) 
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                    (make-branch 8 2))) 
;(b)
(define (structure-is-mobile? structure)
  (pair? structure))
(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (structure-is-mobile? mobile))  mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

;(c)
(define (branch-torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (cond ((null? mobile) true)
        ((not (structure-is-mobile? mobile)) true)
        (else (and (= (branch-torque (left-branch mobile))
                      (branch-torque (right-branch mobile)))
                   (balanced? (branch-structure (left-branch mobile)))
                   (balanced? (branch-structure (right-branch mobile)))))))
;(d)  
(define (left-branch2 mobile)
  (car mobile))
(define (right-branch2 mobile)
  (cdr mobile))
(define (branch-length2 branch)
  (car branch))
(define (branch-structure2 branch)
  (cdr branch))
;;2.30
(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-with-map tree)
  (map (lambda (x) 
         (if (pair? x)
             (square-tree x)
             (square x)))
       tree))
;;2.31
(define (tree-map func tree)
  (map (lambda (x)
         (if (pair? x)
             (tree-map func x)
             (func x)))
       tree))
;;2.32
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
;--------------------------------------------------------------
;2.33~2.39
;--------------------------------------------------------------
;;2.33
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))
(define (new-map p seq)
  (accumulate (lambda (x y) 
                (cons (p x) y))
              null
              seq))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length seq)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              seq))
;;2.34
(define (horner-eval x coefficient-seq)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0 
              coefficient-seq))
;;2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 
                         (if (pair? x)
                             (count-leaves x)
                             1))
                       t)))
;;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
;;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x)
         (dot-product v x)) 
       m))
(define (transpose mat)
  (accumulate-n cons null mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) 
           (matrix-*-vector cols x))
           m)))
;;2.38
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))
;;2.39
(define fold-right accumulate)
(define (reverse1 seq)
  (fold-right (lambda (x y)
                (append y (list x))) 
              null
              seq))
(define (reverse2 seq)
  (fold-left (lambda (x y) 
               (cons y x))
             null
             seq))
;--------------------------------------------------------------
;2.40~2.43
;--------------------------------------------------------------
;2.40
(define (enumerate-interval x y)
  (if (> x y)
      null
      (cons x (enumerate-interval (+ x 1) y))))

(define (unique-pairs n)
  (accumulate append 
              null
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

;********************************************************
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
;********************************************************

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum 
       (filter prime-sum? (unique-pairs n))))
;;2.41
(define (all-different? triple)
  (let ((first (car triple))
        (second (cadr triple))
        (third (caddr triple)))
    (and (not (= first second)) 
         (not (= first third)) 
         (not (= second third)))))
  
(define (trinity n)
 (filter all-different? 
         (accumulate append
                     null
                     (map (lambda (i) 
                            (map (lambda (j) (list i j (- n i j)))
                                 (enumerate-interval 1 (- n i 1))))
                          (enumerate-interval 1 (- n 2))))))
;;2.42
(define empty-board null)
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (cons new-row k))))
(define (safe? k position)
  (define k-row (car (last position)))
  (define (check-ok? n pos)
    (define cur-pos-row (caar pos))
    (cond ((= n k) true)
          ((or (= cur-pos-row k-row);;非同行
               (= cur-pos-row (- k-row (- k n)));;非左上对角线
               (= cur-pos-row (+ k-row (- k n))));;非左下对角线
           false)
          (else (check-ok? (+ n 1) (cdr pos)))))
  (if (< k 2)
      true
      (check-ok? 1 position)))
  
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (accumulate append
                     null
                     (map 
                      (lambda (rest-of-queens)
                        (map (lambda (new-row)
                               (adjoin-position new-row k rest-of-queens))
                             (enumerate-interval 1 board-size)))
                      (queen-cols (- k 1)))))))
  (queen-cols board-size))
;;2.43
(define (queens-slow board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (accumulate append
                     null (map 
                           (lambda (new-row)
                             (map (lambda (rest-of-queens)
                                    (adjoin-position new-row k rest-of-queens))
                                  (queen-cols (- k 1))))
                           (enumerate-interval 1 board-size))))))
  (queen-cols board-size))