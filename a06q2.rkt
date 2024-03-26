;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a06q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    Mann Sunil Gandhi (21038332)
;;    CS 115 Fall 2023
;;    Assignment 6, Question 2
;;*******************************************
;;

;; (weave lst1 lst2 lst3) produces a list of alternating elements
;;  from lst1, lst2 and lst3. 

;; weave: (listof Any) (listof Any) (listof Any) -> (listof Any)

;; Examples:
(check-expect (weave empty empty empty) empty)
(check-expect (weave (list 0 1 5 9) empty empty) (list 0 1 5 9))
(check-expect (weave empty (list 'left 'center 'right) empty)
              (list 'left 'center 'right))
(check-expect (weave empty empty (list "hello" "there" "is"))
              (list "hello" "there" "is"))

(define (weave lst1 lst2 lst3)
  (cond
    [(and (empty? lst1) (empty? lst2) (empty? lst3))
     empty]
    [(and (empty? lst1) (empty? lst2)) lst3]
    [(and (empty? lst1) (empty? lst3)) lst2]
    [(and (empty? lst2) (empty? lst3)) lst1]
    [(empty? lst1)
     (cons (first lst2)
           (cons (first lst3) (weave lst1 (rest lst2)(rest lst3))))]
    [(empty? lst2)
     (cons (first lst1)
           (cons (first lst3) (weave (rest lst1) lst2 (rest lst3))))]
    [(empty? lst3)
     (cons (first lst1)
           (cons (first lst2) (weave (rest lst1) (rest lst2) lst3)))]
    [else (cons (first lst1)
                (cons (first lst2)
                      (cons (first lst3)
                            (weave (rest lst1) (rest lst2) (rest lst3)))))]))

;; Tests:
(check-expect (weave empty (list #\c 1 0.1)
                     (list "cs115" 0.45 22/7 "canada"))
              (list #\c "cs115" 1 0.45 0.1 22/7 "canada"))
(check-expect (weave empty (list 0.88 "pi" #\m 99)
                     (list "euler" "lagrange" 3.14))
              (list 0.88 "euler" "pi" "lagrange" #\m 3.14 99))
(check-expect (weave (list 5 4 3 9 1) empty (list 7 11 56 48))
              (list 5 7 4 11 3 56 9 48 1))
(check-expect (weave (list 5 4 3 9 1) empty (list 7 11 56 48 69))
              (list 5 7 4 11 3 56 9 48 1 69))
(check-expect (weave (list 'q 'p 'local 'user) (list 1 2) empty)
              (list 'q 1 'p 2 'local 'user))
(check-expect (weave (list 21 69) (list "hello" "zeep" "morp") empty)
              (list 21 "hello" 69 "zeep" "morp"))
(check-expect (weave (list 0 2 4 6 8) (list 1 3 5 7 9)
                     (list 1.5 3.5 5.5 7.5 9.5))
              (list 0 1 1.5 2 3 3.5 4 5 5.5 6 7 7.5 8 9 9.5))
(check-expect (weave (list 1 5 8 7 9)
                     (list 2 5 7 4)
                     (list 7 5 1 6))
              (list 1 2 7 5 5 5 8 7 1 7 4 6 9))