;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname a07q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    Mann Sunil Gandhi (21038332)
;;    CS 115 Fall 2023
;;    Assignment 7, Question 1
;;*******************************************
;;

;; (costs-of-ingredients breakfasts) produces a list of the cost of each
;;  breakfast in breakfasts where each breakfast is a two-element list where
;;  the first element in the number of guests and the second element is the
;;  number of pancakes each guest will eat.
;; costs-of-ingredients: (listof (list Nat Nat)) -> (listof Num)
;; Requires:
;;  * the number of guests and number of pancakes should be greater than or
;;     equal to 0.

;; Examples:
(check-expect (costs-of-ingredients empty) empty)
(check-expect (costs-of-ingredients (list (list 5 3))) (list 23.04))
(check-expect (costs-of-ingredients (list (list 10 3)
                                          (list 50 9))) (list 23.04 51.52))

(define (costs-of-ingredients breakfasts)
  (local
    
    [;; Constants related to package size
     (define flour-bag-kg 10)
     (define milk-bag-L 4)
     (define egg-per-carton 12)
     ;; Constants related to amounts used in one batch of pancakes
     (define flour-cups-batch 2)
     (define milk-cups-batch 1.5)
     (define eggs-batch 2)
     ;; Constants related to package price
     (define flour-cost 12.78)
     (define milk-cost 6.28)
     (define egg-cost 3.98)
     ;; Constants related to converting package sizes to cups
     (define flour-cup 0.125)
     (define milk-cup 0.250)
     (define flour-cups-per-package (/ flour-bag-kg flour-cup))
     (define milk-cups-per-package (/ milk-bag-L milk-cup))
     ;; Number of pancakes per batch
     (define pancakes-per-batch 16)

     ;; (batches to-serve per-person) produces the number of batches of pancakes
     ;;  needed so that to-serve people can each have per-person pancakes.
     ;; batches: Nat Nat -> Nat
     
     (define (batches to-serve per-person)
       (ceiling (/ (* to-serve per-person) pancakes-per-batch)))

     ;; (packages-needed num-batches per-batch package-size) produces the number
     ;;  of packages of the ingredient needed to make num-batches batches of
     ;;  pancakes, given that one batch of pancakes requires per-batch units of
     ;;  the ingredient and the package contains package-size units of the
     ;;  ingredient.
     ;; packages-needed: Nat Num Num -> Nat
     ;; Requires: 0 < per-batch, package-size
     
     (define (packages-needed num-batches per-batch package-size)
       (ceiling (/ (* num-batches per-batch) package-size)))

     ;; (total-cost to-serve per-person) produces the cost of
     ;;  ingredients to make pancakes so that to-serve people can each have
     ;;  per-person pancakes.
     ;; total-cost: Nat Nat -> Num
     
     (define (total-cost to-serve per-person)
       (+ (* egg-cost (packages-needed
                       (batches to-serve per-person)
                       eggs-batch
                       egg-per-carton))
          (* flour-cost (packages-needed
                         (batches to-serve per-person)
                         flour-cups-batch
                         flour-cups-per-package))
          (* milk-cost (packages-needed
                        (batches to-serve per-person)
                        milk-cups-batch
                        milk-cups-per-package))))]
    (cond
      [(empty? breakfasts) empty]
      [else (cons
             (total-cost
              (first (first breakfasts))
              (second (first breakfasts)))
             (costs-of-ingredients (rest breakfasts)))])))

;; Tests:
(check-expect (costs-of-ingredients (list
                                     (list 5 3)
                                     (list 10 20))) (list 23.04 37.28))
(check-expect (costs-of-ingredients (list
                                     (list 100 10)
                                     (list 5000 23))) (list 107.02 11301.16))
(check-expect (costs-of-ingredients (list (list 1 1))) (list 23.04))
(check-expect (costs-of-ingredients (list (list 7 1) (list 5 100)))
              (list 23.04 55.5))