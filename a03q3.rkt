;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a03q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    CS 115 Fall 2023
;;    Assignment 3, Question 3
;;*******************************************
;;

;; (convert-to-24-hours hour type) converts 12 hour time to 24 hour
;;  format
;; convert-to-24-hours: Nat Sym -> Nat

;; Examples:
(check-expect (convert-to-24-hours 2 'PM) 14)
(check-expect (convert-to-24-hours 9 'AM) 9)
(check-expect (convert-to-24-hours 11 '24H) 11)

(define (convert-to-24-hours hour type)
    
    (cond
      [(and (symbol=? type 'AM) (= hour 12)) 0]
      [(symbol=? type 'PM) (cond
                             [(= hour 12) hour]
                             [else (+ hour 12)])]
      [else hour]))

;; (time<=? hour1 minute1 type1 hour2 minute2 type) produces true if
;; the first time is not later than the second one, else produces false.
;; time<=?: Nat Nat Sym Nat Nat Sym -> Bool
;; Requires:
;;  hour1,hour2 are less than 24
;;  minute1,minute are less than 60
;;  type1,type2 to be 'AM,'PM or '24H.

;;Examples:
(check-expect (time<=? 11 30 'AM 15 20 '24H) true)
(check-expect (time<=? 2 18 'PM 9 55 'AM) false)
(check-expect (time<=? 10 14 'AM 10 14 '24H) true)

(define (time<=? hour1 minute1 type1 hour2 minute2 type2)
  
  (cond
    [(and (= (convert-to-24-hours hour1 type1)
             (convert-to-24-hours hour2 type2))
          (> minute1 minute2))
     false]
    [(and (= (convert-to-24-hours hour1 type1)
             (convert-to-24-hours hour2 type2))
          (> minute2 minute1))
     true]
    [(>= (convert-to-24-hours hour2 type2)
         (convert-to-24-hours hour1 type1))
     true]
    [(> (convert-to-24-hours hour1 type1)
         (convert-to-24-hours hour2 type2))
     false]))

;; Tests:
(check-expect (time<=? 12 30 'AM 12 30 'PM) true)
(check-expect (time<=? 12 30 'AM 12 31 'AM) true)
(check-expect (time<=? 11 24 '24H 13 31 '24H) true)
(check-expect (time<=? 3 30 'PM 15 31 '24H) true)
(check-expect (time<=? 3 31 'PM 3 30 'PM) false)