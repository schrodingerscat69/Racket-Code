;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a01q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (heart-y x)
  (+ (sqrt(- 36 (sqr x))) (/ (- (+ (* 2 (abs x)) (sqr x)) 6) (+ (+ (* 3 (abs x)) (sqr x)) 2
                                                                ))))