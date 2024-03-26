;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a06q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    Mann Sunil Gandhi (21038332)
;;    CS 115 Fall 2023
;;    Assignment 6, Question 3
;;*******************************************
;;

;; (position-char) produces a list of the indices of the characters
;;  in lst if it is equal to char, a character, else produces an empty
;;  list.
;; position-char: Char (listof Char) Nat -> (listof Nat)
;; Requires:
;;  * index to be initialized to zero.

;; Examples:
(check-expect (position-char #\a (list #\a) 0) (list 0))
(check-expect (position-char #\b empty 0) empty)
(check-expect (position-char #\a (list #\a #\b #\a) 0) (list 0 2))

(define (position-char char lst index)
  (cond
    [(empty? lst) empty]
    [(char=? char (first lst))
     (cons index (position-char char (rest lst) (add1 index)))]
    [else (position-char char (rest lst) (add1 index))]))

;; (make-pair index lst-pos) produces a list of pairs of indices
;;  by adding index to each number in lst-pos.
;; make-pair: Nat (listof Nat) -> (listof (list Nat Nat))
;; Requires:
;;  * index to be initialized to zero

;;Examples:
(check-expect (make-pair 1 (list 1)) (list (list 1 1)))
(check-expect (make-pair 2 empty) empty)
(check-expect (make-pair 1 (list 2 3 4))
              (list (list 1 2) (list 1 3) (list 1 4)))

(define (make-pair index lst-pos)
  (cond
    [(empty? lst-pos) empty]
    [else (cons (list index (first lst-pos))
                (make-pair index (rest lst-pos)))]))

;; (process-list lst1 lst2 index) produces a list of pairs of indices if
;;  the character at a specific index in lst1 is equal to a character at a
;;  specific index in lst2, else produces an empty list.
;; process-list: (listof Char) (listof Char) Nat -> (listof Nat)
;; Requires:
;;  * index to be initialzed to zero

;; Examples:
(check-expect (process-list empty (list #\t #\e #\s #\t) 0) empty)
(check-expect (process-list (list #\a) empty 0) empty)
(check-expect (process-list (list #\a #\b) (list #\b #\a) 0)
              (list (list 0 1) (list 1 0)))


(define (process-list lst1 lst2 index)
  (cond
    [(empty? lst1) empty]
    [else (append (make-pair index (position-char (first lst1) lst2 0))
                  (process-list (rest lst1) lst2 (add1 index)))]))

;; (crossfit word1 word2) produces a list of a pair of numbers representing
;;  the indices at which a character in the string word1 is equal to a charater
;;  in word2.
;; crossfit: Str Str -> (listof (list Nat Nat))
;; Requires:
;;  * word1 and word2 to be strings of entirely lowercase alphabetic characters

;; Examples:
(check-expect (crossfit "" "test") empty)
(check-expect (crossfit "a" "") empty)
(check-expect (crossfit "tomato" "camel")
              (list (list 2 2) (list 3 1)))
(check-expect (crossfit "folly" "mulled")
              (list (list 2 2) (list 2 3)
                    (list 3 2) (list 3 3)))
(check-expect (crossfit "foil" "paper") empty)

(define (crossfit word1 word2)
  (process-list (string->list word1) (string->list word2) 0))

;; Tests:
(check-expect (crossfit "a" "a") (list (list 0 0)))
(check-expect (crossfit "abc" "def") empty)
(check-expect (crossfit "ABC" "abc") empty)
(check-expect (crossfit "apple" "plane")
              (list (list 0 2) (list 1 0) (list 2 0) (list 3 1) (list 4 4)))
(check-expect (crossfit "test" "text")
              (list (list 0 0) (list 0 3) (list 1 1) (list 3 0) (list 3 3)))
(check-expect (crossfit "at" "cat") (list (list 0 1) (list 1 2)))
(check-expect (crossfit "cat" "at") (list (list 1 0) (list 2 1)))
(check-expect (crossfit "Abc" "abc") (list (list 1 1) (list 2 2)))  
(check-expect (crossfit "Abc" "aBc") (list (list 2 2)))  
(check-expect (crossfit "moon" "room")
              (list (list 0 3) (list 1 1) (list 1 2)
                    (list 2 1) (list 2 2)))
(check-expect (crossfit "bubble" "trouble")
              (list (list 0 4) (list 1 3) (list 2 4)
                    (list 3 4) (list 4 5) (list 5 6)))