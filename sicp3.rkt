#! /usr/bin/env racket
#lang racket

(define (make-account balance)
  (let* ((withdraw 
          (lambda (amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                       balance)
                "Lack of balance")))
         (deposit 
          (lambda (amount) 
            (begin (set! balance (+ balance amount))
                   balance)))
         (dispatch 
          (lambda (m)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Eror balance" m))))))
    dispatch))

(define (monte-carlo trials experiment)
  (letrec ((iter 
          (lambda (trials-remaining trials-passed)
            (cond ((= trials-remaining 0)
                   (/ trials-passed trials))
                  (else 
                   (iter (- trials-remaining 1)
                         (+ trials-passed
                            (if (experiment) 1 0))))))))
    (iter trials 0)))
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (random #e1e5) (random #e1e5)) 1))
(estimate-pi 4e4)
         
