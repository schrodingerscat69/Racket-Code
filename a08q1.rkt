;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a08q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    Mann Sunil Gandhi (21038332)
;;    CS 115 Fall 2023
;;    Assignment 8, Question 1
;;*******************************************
;;

;; (cookie-sizes dought-amounts threshold) produces a list of the size of
;;  cookies (which must be perfect squares) that can be made from each amount
;;  in the list dough-amounts without wasting more than threshold dough and
;;  without requiring more than threshold extra dough.
;; cookie-sizes: (listof Nat) Nat -> (listof Nat)

;; Examples:
(check-expect (cookie-sizes empty 100) empty)
(check-expect (cookie-sizes (list 2 25 22 18) 2)
              (list 1 25 16))
(check-expect (cookie-sizes (list 31 41 42 36) 5)
              (list 36 36 36))

(define (cookie-sizes dough-amounts threshold)
  (local
    [;; (closest-perfect-square n) produces the perfect square that is closest
     ;;  to n.
     ;; closest-perfect-square: Nat -> Nat
     
     (define (closest-perfect-square n)
       (inexact->exact (sqr (round (sqrt n)))))

     ;; (within-threshold? num) produces true if the closest perfect square is
     ;;  less than or equal to threshold steps away from num, else produces
     ;;  false. 
     ;; within-threshold?: Nat -> Bool
     
     (define (within-threshold? num)
       (<= (abs (- num (closest-perfect-square num)))
           threshold))]
    
    (map closest-perfect-square (filter within-threshold? dough-amounts))))

;; Tests: 
(check-expect (cookie-sizes (list 7 25 22 17) 2)
              (list 9 25 16))
(check-expect (cookie-sizes (list 17 21 24) 0) empty)
(check-expect (cookie-sizes (list 93 91 92) 5) empty)
(check-expect (cookie-sizes (list 23 54 99) 2)
              (list 25 100))
(check-expect (cookie-sizes (list 0 81 16) 0)
              (list 0 81 16))
(check-expect (cookie-sizes (list 18 14) 3)
              (list 16 16))
(check-expect (cookie-sizes (list 1 2 3 4 5 6 7 8 9 10) 1)
              (list 1 1 4 4 4 9 9 9))