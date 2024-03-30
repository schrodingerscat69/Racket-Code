;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a07q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    CS 115 Fall 2023
;;    Assignment 7, Question 2
;;*******************************************
;;

;; (outliers lon) produces a list of numbers that are outliers in lon.
;; outliers: (listof Num) -> (listof Num)

;; Examples:
(check-expect (outliers empty) empty)
(check-expect (outliers (list 5)) empty)
(check-expect (outliers (list 10 2 3 4 5 6 7 8 9 100)) (list 100))

(define (outliers lon)
  (local
    
    [;; (sum lon) produces the sum of numbers in lon.
     ;; sum: (listof Num) -> Num
     
     (define (sum lon)
       (cond
         [(empty? lon) 0]
         [else (+ (first lon) (sum (rest lon)))]))

     ;; (mean lon) produces the mean of the numbers in lon.
     ;; mean: (listof Num) -> Num
     
     (define (mean lon)
       (/ (sum lon) (length lon)))
     
     ;; (variance lst1 lst2) produces the sum of variances of each number in
     ;;  lst1 while lst2 goes along for the ride to calculate the mean of lst2
     ;; variance: (listof Num) (listof Num) -> Num
     ;; Requires:
     ;;  * lst1 and lst2 to be equal
     
     (define (variance lst1 lst2)
       (cond
         [(empty? lst1) 0]
         [else (+ (expt (- (first lst1) (mean lst2)) 2)
                  (variance (rest lst1) lst2))]))

     ;; (variance-lon lon) produces the variance of the list lon.
     ;; variance-lon: (listof Num) -> Nat
     
     (define (variance-lon lon)
       (/ (variance lon lon) (length lon)))

     ;; (is-outlier? x lst) produces true if the number x in the list lst
     ;;  is an outlier, else produces false
     ;; is-outlier?: Num (listof Num) -> Bool
     
     (define (is-outlier? x lst)
       (> (expt (- x (mean lst)) 2) (variance-lon lst)))

     ;; (outlier-helper lst1 lst2) produces a list of numbers that are outliers
     ;;  in lst1 while lst2 goes along for the ride. 
     ;; outlier-helper: (listof Num) (listof Num) -> (listof Num)
     ;; Requires:
     ;;  * lst1 and lst2 to be equal
     
     (define (outlier-helper lst1 lst2)
       (cond
         [(empty? lst1) empty]
         [(is-outlier? (first lst1) lst2)
          (cons (first lst1) (outlier-helper (rest lst1) lst2))]
         [else (outlier-helper (rest lst1) lst2)]))]
    
    (outlier-helper lon lon)))

;; Tests:
(check-expect (outliers (list 1 2 3 4 5 6 7 8 9 10)) (list 1 2 9 10))
(check-expect (outliers (list 2 1 34 3 1 0 33 2 2 1 34 3)) (list 34 33 34))
(check-expect (outliers (list -1.5 1.5)) empty)
(check-expect (outliers (list -100 78 56 -42 89 74 -65 31 20 95))
              (list -100 -65 95))
(check-expect (outliers (list 69 69 69 69 69)) empty)
(check-expect (outliers (list 1 2000 2 2000 3 2000)) (list 1))