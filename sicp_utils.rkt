#lang racket

(provide dx)
(provide tolerance)

(provide identify)
(provide inc)
(provide dec)
(provide square)
(provide cube)
(provide halve)
(provide double)
(provide average)
(provide close-enough?)
(provide divides?)
(provide runtime)

(define dx 0.0001)
(define tolerance 0.00001)

;;取本身操作
(define (identify x) x)
;;加一操作
(define (inc x) (+ x 1))
;;减一操作
(define (dec x) (- x 1))
;;求平方操作
(define (square x) (* x x))
;;求立方操作
(define (cube x) (* x x x))
;;折半
(define (halve x) (/ x 2))
;;翻倍
(define (double x) (* x 2))
;;求两数平均
(define (average x y) (/ (+ x y) 2))
;;两个数是否足够近
(define (close-enough? v1 v2)
  (< (/ (abs (- v1 v2)) v2) tolerance)) 
;;b是否可以整除a
(define (divides? a b)
  (= (remainder a b) 0))
;;计算运行时间
(define (runtime) (current-inexact-milliseconds))