;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a06q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    Mann Sunil Gandhi (21038332)
;;    CS 115 Fall 2023
;;    Assignment 6, Question 1
;;*******************************************
;;

;; (secret los lop) produces a string by matching a string of
;;  length 1 in los to a natural number is lop.

;; secret: (listof Str) (listof Nat) -> Str
;; Requires:
;;  * each string in los to have length 1.
;;  * each position in lop is a valid position in los

;; Examples:
(check-expect (secret (list "a" "n" "m" "h" "c" "e" "s" "t" "r")
                      (list 2 0 1 4 3 5 6 7 5 8)) "manchester")
(check-expect (secret (list "l" "o" "h" "e") empty) "")
(check-expect (secret (list "p") (list 0)) "p")
(check-expect (secret (list "o" "y" "z") (list 0 2)) "oz")

(define (secret los lop)
  (cond
    [(empty? lop) ""]
    [else (string-append (list-ref los (first lop))
                         (secret los (rest lop)))]))

;; Tests:
(check-expect (secret (list "a" "b" "c") empty) "")
(check-expect (secret (list "l" "d" "z") (list 1 1 1)) "ddd")
(check-expect (secret (list "h" "w" "a" "s" "t" "'" " " "p" "u" "?")
                      (list 1 0 2 4 5 3 6 8 7 9)) "what's up?")
(check-expect (secret (list "r" "e" "o") (list 1 0 0 2 0)) "error")
