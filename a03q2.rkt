;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a03q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    Mann Sunil Gandhi (21038332)
;;    CS 115 Fall 2023
;;    Assignment 3, Question 2
;;*******************************************
;;

;; (match-winner results) produces the country of the winner
;;  "AAA", "BBB" or "DRAW".
;; match-winner: Str -> Str
;; Requires:
;;  length of results to be 11. 

;;Examples:
(check-expect (match-winner "CAN 4-3 USA") "CAN")
(check-expect (match-winner "ENG 0-2 FRA") "FRA")
(check-expect (match-winner "FRA 1-1 CAN") "DRAW")

(define (match-winner results)
  
  (cond
    [(> (string->number (string-ith results 4))
        (string->number (string-ith results 6)))
     (substring results 0 3)]
    [(< (string->number (string-ith results 4))
        (string->number (string-ith results 6)))
     (substring results 8 11)]
    [else "DRAW"]))

;; Tests:
(check-expect (match-winner "GER 1-0 FRA") "GER")
(check-expect (match-winner "CRO 2-4 FRA") "FRA")
(check-expect (match-winner "ARG 3-3 FRA") "DRAW")
(check-expect (match-winner "BRA 7-0 RUS") "BRA")