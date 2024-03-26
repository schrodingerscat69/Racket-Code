;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a01q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (cost-of-ingredients to-serve per-person)
  (+ (* 12.78 (ceiling (/ (* 0.25 (ceiling (/ (* to-serve per-person) 16))) 10))) (* 6.28 (ceiling (/ (* 0.375 (ceiling (/ (* to-serve per-person) 16))) 4))) (* 3.98 (ceiling (/ (* 2 (ceiling (/ (* to-serve per-person) 16))) 12)))))