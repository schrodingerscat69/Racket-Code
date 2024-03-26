;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a01q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (needed-exam-grade quizzes assignments midterm)
  (max (/ (- 50 (* 0.2 quizzes) (* 0.3 assignments) (* 0.1 midterm)) 0.4) (/ (- 25 (* 0.1 midterm)) 0.4)))
    