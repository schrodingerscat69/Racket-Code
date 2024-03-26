;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    Mann Sunil Gandhi (21038332)
;;    CS 115 Fall 2023
;;    Assignment 9, Question 1
;;*******************************************
;;

(define-struct robot (name cost weight speed wheels?))
;; A Robot is a (make-robot Str Num Num Num Bool)
;; Requires:
;;  * name is a non-empty string
;;  * cost and speed are non-negative numbers
;;  * weight is a positive number
;; Where:
;;  * wheels? is true if the robot has wheels and false otherwise

;; (combine-robots r1 r2) produces a new robot by combining robot r1 and
;;  robot r2.
;; combine-robots: Robot Robot -> Robot

;; Examples:
(check-expect (combine-robots (make-robot "Hal" 20 50 150.65 false)
                              (make-robot "Jordan" 45 30 125.5 false))
              (make-robot "Hal Jordan" 32.5 80 125.5 false))
(check-expect (combine-robots (make-robot "Hal" 20 50 150.65 true)
                              (make-robot "Jordan" 45 30 125.5 false))
              (make-robot "Hal Jordan" 32.5 80 125.5 true))

(define (combine-robots r1 r2)
  (local
    [;; (has-wheels? r1 r2) produces true if either robot r1 or r2 has wheels,
     ;;  else produces false.
     ;; has-wheels?: Robot RObot -> Bool
     
     (define (has-wheels? r1 r2)
       (or (robot-wheels? r1)
           (robot-wheels? r2)))
     
     ;; (new-speed? r1 r2) produces the minimum of the speed of the robots
     ;;  r1 and r2.
     ;; new-speed: RObot Robot -> Num
     
     (define (new-speed? r1 r2)
       (min (robot-speed r1) (robot-speed r2)))

     ;; (new-weight? r1 r2) produces the combined weight of the robots r1
     ;;  and r2.
     ;; new-weight?: Robot Robot -> Num
     
     (define (new-weight? r1 r2)
       (+ (robot-weight r1) (robot-weight r2)))

     ;; (new-cost? r1 r2) produces the average of the cost of the two robots
     ;;  r1 and r2.
     ;; new-cost?: Robot Robot -> Num
     
     (define (new-cost? r1 r2)
       (/ (+ (robot-cost r1) (robot-cost r2)) 2))

     ;; (new-name? r1 r2) produces a string with the names of the robots r1
     ;;  and r2 with a space between them.
     ;; new-name?: Robot Robot -> Str
     
     (define (new-name? r1 r2)
       (string-append (robot-name r1) " " (robot-name r2)))]
    
    (make-robot (new-name? r1 r2)
                (new-cost? r1 r2)
                (new-weight? r1 r2)
                (new-speed? r1 r2)
                (has-wheels? r1 r2))))

;; Tests:
(define robot1 (make-robot "Alpha" 1000 20 5 true))
(define robot2 (make-robot "Beta" 2000 25 10 false))
(define robot3 (make-robot "Gamma" 1500 30 15 true))
(define robot4 (make-robot "Delta" 500 10 20 false))
(define bob (make-robot "Bob" 34.99 100 56.5 true))
(define loblaw (make-robot "Loblaw" 65.01 96.2 192.3 false))
(check-expect (combine-robots bob loblaw)
              (make-robot "Bob Loblaw" 50 196.2 56.5 true))
(check-expect (combine-robots robot1 robot2)
              (make-robot "Alpha Beta" 1500 45 5 true))
(check-expect (combine-robots robot1 robot3)
              (make-robot "Alpha Gamma" 1250 50 5 true))
(check-expect (combine-robots robot2 robot3)
              (make-robot "Beta Gamma" 1750 55 10 true))
(check-expect (combine-robots robot2 robot4)
              (make-robot "Beta Delta" 1250 35 10 false))
(check-expect (combine-robots robot1 robot4)
              (make-robot "Alpha Delta" 750 30 5 true))