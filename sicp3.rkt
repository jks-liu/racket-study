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
