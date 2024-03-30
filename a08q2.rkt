;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a08q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    CS 115 Fall 2023
;;    Assignment 8, Question 2
;;*******************************************
;;

;; (find pred lst) produces the index of the first element in lst
;;  (starting from 0) which satisfies the predicate pred, or false
;;  if no such element is found in lst.
;; find: (Any Any -> Bool) (listof Any) -> (anyof Nat false)

;; Examples:
(check-expect (find even? empty) false)
(check-expect (find odd? (list 2 4 6 8 10)) false)
(check-expect (find even? (list 2 4 6)) 0)
(check-expect (find even? '(3 4 7 11)) 1)
(check-expect (find symbol? (list "not" "a" "symbol" 'thisone )) 3)

(define (find pred lst)
  (local
    [;; (index-finder index lst) produces the index index of the first element
     ;;  in lst which satifies the predicate pred, else false.
     ;; index-finder: Nat (listof Any) -> (anyof Nat false)
     
     (define (index-finder index lst)
       (cond
         [(empty? lst) false]
         [(pred (first lst)) index]
         [else (index-finder (add1 index) (rest lst))]))]
    (index-finder 0 lst)))

;; Tests:
(check-expect (find negative? (list 0 1 -7 2 3 -4 4)) 2)
(check-expect (find symbol? (list "a" true 17)) false)
(check-expect (find (lambda (x) (char=? x #\g))
                    (string->list "cargo shorts"))
              3)
(check-expect (find positive? (list -2 -9 2 -1)) 2)
(check-expect (find (lambda (x) (symbol=? x 'left))
                    (list 'right 'center 'right 'left)) 3)
(check-expect (find (lambda (x) (> x 50))
                    (list 20 pi 41 100000000)) 3)
(check-expect (find (lambda (x) (> (length (string->list x)) 4))
                    (list "hi" "gold" "hello!" "hamburger")) 2)