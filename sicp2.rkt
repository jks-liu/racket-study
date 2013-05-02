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

(define (deriv exp var)
  (let* 
      ((variable? (lambda (v) (symbol? v)))
       (same-variable? 
        (lambda (v0 v1)
          (and (variable? v0)
               (variable? v1)
               (eq? v0 v1))))
       (sum? (lambda (v) 
               (and (pair? v)
                    (eq? (car v) '+))))
       (product? (lambda (v)
                   (and (pair? v)
                        (eq? (car v) '*))))
       (addend (lambda (v) (cadr v)))
       (augend (lambda (v) (caddr v)))
       (multiplier (lambda (v) (cadr v)))
       (multiplicand (lambda (v) (caddr v)))
       (make-sum (lambda (v0 v1) (list '+ v0 v1)))
       (make-product (lambda (v0 v1) (list '* v0 v1))))
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var)))
          ((product? exp)
           (make-sum 
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (multiplicand exp)
                          (deriv (multiplier exp) var))))
          (else (error "Error deriv" exp)))))

(define (prettify v)
  (let* 
      ((variable? (lambda (v) (symbol? v)))
       (same-variable? 
        (lambda (v0 v1)
          (and (variable? v0)
               (variable? v1)
               (eq? v0 v1))))
       (sum? (lambda (v) 
               (and (pair? v)
                    (eq? (car v) '+))))
       (product? (lambda (v)
                   (and (pair? v)
                        (eq? (car v) '*))))
       (addend (lambda (v) (cadr v)))
       (augend (lambda (v) (caddr v)))
       (multiplier (lambda (v) (cadr v)))
       (multiplicand (lambda (v) (caddr v)))
       (make-sum (lambda (v0 v1) (list '+ v0 v1)))
       (make-product (lambda (v0 v1) (list '* v0 v1))))
    (cond ((or (variable? v) (number? v)) v)
          ((sum? v)
           (cond ((eq? (addend v) 0) (augend v))
                 ((eq? (augend v) 0) (addend v))
                 ((and (number? (addend v))
                       (number? (augend v))) 
                  (+ (addend v) (augend v)))
                 (else 0))); wrong here
          ((product? v)
           (cond ((or (eq? (multiplier v) 0)
                      (eq? (multiplicand v) 0)) 0)
                 ((eq? (multiplier v) 1) (multiplicand v))
                 ((eq? (multiplicand v) 1) (multiplier v))
                 ((and (number? (multiplier v))
                       (number? (multiplicand v)))
                  (* (multiplier v) (multiplicand v)))
                 (else (error "Error prttify." v))))))) ; wrong here

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? l)
  (eq? (car l) 'leaf))
(define (symbol-leaf l)
  (cadr l))
(define (weight-leaf l)
  (caddr l))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))



