#! /usr/bin/env racket
#lang racket

(define gcd 
  (lambda (a b) 
    (if (= b 0)
        a
        (gcd b (remainder a b)))))

(for-each (lambda (i) (display i)) 
          `("GCD of 88 and 55 is: " ,(gcd 88 55) ".\n"))

(define exp-mod
  (lambda (base exp m)
    (define square (lambda (x) (* x x)))
    (cond ((= exp 0) 1)
          ((even? exp) (remainder 
                        (square 
                         (exp-mod base (/ exp 2) m))
                        m))
          (else (remainder 
                 (* base (exp-mod base (- exp 1) m))
                 m)))))

(exp-mod 7 11 11)