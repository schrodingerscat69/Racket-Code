;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a05q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (factorial x)
  (cond
    [(zero? x) 1]
    [else (* x (factorial (sub1 x)))]))

(define (calculate-term z)
  (/ (* (expt 2 (+ z 1)) (expt (factorial z) 2))
     (factorial (+ (* 2 z) 1))))

(define (approximate-pi n)
  (cond
    [(zero? n) 2]
    [else (+ (calculate-term n)
             (approximate-pi (sub1 n)))]))