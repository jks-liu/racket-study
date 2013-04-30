#! /usr/bin/env racket
#lang racket

(display "we forget about what the symbols stand for.\n")
(define count-leaves
  (lambda (x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x))
                   (count-leaves (cdr x)))))))
(count-leaves '(1 (2 3)))

(define map-tree
  (lambda (tree function)
    (cond ((null? tree) '())
          ((not (pair? tree)) (function tree))
          (else (cons (map-tree 
                       (car tree) function)
                      (map-tree
                       (cdr tree) function))))))
(map-tree '((2 4) (3 (0 9))) (lambda (x) (* 10 x)))

(display "enumerate, filter, map, accumulate.\n")
(define filter 
  (lambda (predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence))
           (cons (car sequence)
                 (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))) 
(filter (lambda (x) (= 0 (remainder x 3)))
        '(1 2 3 4 5 6 7 8 9 0))

(define accumulate
  (lambda (operate initial sequence)
    (if (null? sequence)
        initial
        (accumulate operate 
                    (operate initial (car sequence))
                    (cdr sequence)))))
(accumulate + 0 '(1 2 3 4 5))

(display "The world-enforced distinction
         between the practical and the scientific worker
         is utterly futile,
         and the whole experience of modern times
         has demonstrated its utter worthlessness.\n")


