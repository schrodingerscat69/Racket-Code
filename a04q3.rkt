;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a04q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    CS 115 Fall 2023
;;    Assignment 4, Question 3
;;*******************************************
;;

;; (char->value char) produces the value that the
;;  roman numeral char takes.
;; char->value: Char -> Nat

;; Examples:
(check-expect (char->value #\m) 0)
(check-expect (char->value #\B) 0)
(check-expect (char->value #\M) 1000)
(check-expect (char->value #\L) 50)

(define (char->value char)
  (cond
    [(char=? char #\M) 1000]
    [(char=? char #\D) 500]
    [(char=? char #\C) 100]
    [(char=? char #\L) 50] 
    [(char=? char #\X) 10]
    [(char=? char #\V) 5]
    [(char=? char #\I) 1] 
    [else 0]))

;; (convert-from-roman lst total) produces the numeric value total of
;;  the roman numeral characters in the list lst.
;; convert-from-roman: (listof Char) Nat -> Nat
;; Requires:
;;  * lst to be a list of characters
;;  * total must be initially 0

;; Examples:
(check-expect (convert-from-roman empty 0) 0)
(check-expect (convert-from-roman (cons #\X empty) 0) 10)
(check-expect (convert-from-roman (cons #\X
                                      (cons #\C
                                            (cons #\I
                                                  (cons #\X empty)))) 0) 99)
               
(define (convert-from-roman lst total)
  (cond
    [(empty? lst) total]
    [(empty? (rest lst)) (+ total (char->value (first lst)))]
    [(>= (char->value (first lst)) (char->value (second lst)))
     (convert-from-roman (rest lst) (+ total (char->value (first lst))))]
    [else
     (convert-from-roman (rest lst) (- total (char->value (first lst))))]))

;; (roman->number rnum) produces the total numeric value represented by
;;  the roman number passed as the string rnum.
;; roman->number: Str -> Nat
;; Requires:
;;  * rnum to be a string of letters.

;; Examples:
(check-expect (roman->number "") 0)
(check-expect (roman->number "CCC") 300)
(check-expect (roman->number "XL") 40)

(define (roman->number rnum)
  (convert-from-roman (string->list rnum) 0))

;; Tests:
(check-expect (roman->number "") 0)
(check-expect (roman->number "randomstring") 0)
(check-expect (roman->number "MDCCXVII") 1717)
(check-expect (roman->number "MMCCXXII") 2222)
(check-expect (roman->number "LV") 55)