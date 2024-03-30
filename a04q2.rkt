;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a04q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    CS 115 Fall 2023
;;    Assignment 4, Question 2
;;*******************************************
;;

;; (rounded n) produces the floor of the square root of n.
;; rounded: Num -> Nat

;; Examples:
(check-expect (rounded 5) 2)
(check-expect (rounded 14) 3)
(check-expect (rounded 100) 10)

(define (rounded n)
  (inexact->exact (floor (sqrt n))))

;; (square-of-n n) produces the first perfect square less than
;;  or equal to n.
;; square-of-n: Nat -> Nat

;; Examples:
(check-expect (square-of-n 0) 0)
(check-expect (square-of-n 5) 4)
(check-expect (square-of-n 28) 25)
(check-expect (square-of-n 69) 64)

(define (square-of-n n)
  (expt (rounded n) 2))

;; (square-of-next n) produces the first perfect square greater than n.
;; square-of-next: Nat -> Nat

;; Examples:
(check-expect (square-of-next 0) 1)
(check-expect (square-of-next 5) 9)
(check-expect (square-of-next 130) 144)
 
(define (square-of-next n)
  (expt (+ (rounded n) 1) 2))

;; (closest-square? n) produces the perfect square closest to n.
;; closest-square?: Nat -> Nat

;; Examples:
(check-expect (closest-square? 0) 0)
(check-expect (closest-square? 5) 4)
(check-expect (closest-square? 13) 16)

(define (closest-square? n)
  (cond
    [(<= (- n (square-of-n n)) (abs(* -1 (- (square-of-next n) n))))
     (square-of-n n)]
    [else (square-of-next n)]))

;; (cookie-sizes dough-amounts threshold) produces a list of perfect squares
;;  closest to each natural number in the list dough-amounts, omitting the
;;  perfect squares that are greater than the threshold.
;; cookie-sizes: (listof Nat) Nat -> (listof Nat)
;; Requires:
;;  * dough-amounts to be a list of natural numbers or empty
;;  * threshold to be a natural number

;; Examples:
(check-expect (cookie-sizes (cons 7 (cons 25 (cons 22 (cons 17 empty)))) 2)
              (cons 9 (cons 25 (cons 16 empty))))
(check-expect (cookie-sizes (cons 17 (cons 21 (cons 24 empty))) 0) empty)
(check-expect (cookie-sizes (cons 15 (cons 33 empty)) 3)
              (cons 16 (cons 36 empty)))

(define (cookie-sizes dough-amounts threshold)
  (cond
    [(empty? dough-amounts) empty]
    [(>= threshold (abs (* -1 (- (closest-square? (first dough-amounts))
                 (first dough-amounts)))))
     (cons (closest-square? (first dough-amounts))
           (cookie-sizes (rest dough-amounts) threshold))]
    [else (cookie-sizes (rest dough-amounts) threshold)]))

;; Tests:
(check-expect (cookie-sizes empty 10000) empty)
(check-expect (cookie-sizes (cons 34 (cons 31 (cons 99 empty))) 4)
              (cons 36 (cons 100 empty)))
(check-expect (cookie-sizes (cons 100 empty) 0) (cons 100 empty)) 
(check-expect (cookie-sizes (cons 150 (cons 223 (cons 498 empty))) 15)
              (cons 144 (cons 225 (cons 484 empty))))
