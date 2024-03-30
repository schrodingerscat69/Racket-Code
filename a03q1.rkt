;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a03q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    CS 115 Fall 2023
;;    Assignment 3, Question 1
;;*******************************************
;;

;; (block-sum digits block-size) produces the sum of the
;;  digits depending on how to break the string. 
;; block-sum: Str Num -> Num
;; Requires:
;;   length of digits to be 4
;;   block-size can take the values 1,2 or 4 only.

;;Examples:
(check-expect (block-sum "1234" 4) 1234)
(check-expect (block-sum "1234" 2) 46)

(define (block-sum digits block-size)
  
  (cond
    [(= block-size 4) (string->number digits)]
    [(= block-size 2)
     (+ (string->number (substring digits 0 2))
        (string->number (substring digits 2 4)))]
    [(= block-size 1)
     (+ (string->number (substring digits 0 1))
        (string->number (substring digits 1 2))
        (string->number (substring digits 2 3))
        (string->number (substring digits 3 4)))]))

;; Tests:
(check-expect (block-sum "0000" 4) 0)
(check-expect (block-sum "0000" 2) 0)
(check-expect (block-sum "0000" 1) 0)
(check-expect (block-sum "2169" 4) 2169)
(check-expect (block-sum "2169" 2) 90)
(check-expect (block-sum "2169" 1) 18)