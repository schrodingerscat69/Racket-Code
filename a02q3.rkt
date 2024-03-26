;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a02q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (simplified-yahtzee-score d1 d2 d3 d4 d5)
  (cond
    [(= d1 d2 d3 d4 d5) 50]
    [(and (= d2 d3 d4) (not (equal? d1 d5))) (* 3 d2)]
    [(and (= d1 d2 d3) (not (equal? d4 d5))) (* 3 d2)]
    [(or (and (= d1 d2) (= d3 d4 d5)) (and (= d1 d2 d3) (= d4 d5))) 25]
    [(= d1 d2 d3 d4) (* 4 d1)]
    [(= 1 (- d5 d4) (- d4 d3) (- d3 d2) (- d2 d1)) 40]
    [(and (= d1 d2) (= d3 d4)) (max (* 2 d1) (* 2 d3))]
    [(and (= d1 d2 d3) (not (equal? d4 d5))) d5]
    [(and (= d3 d4) (not (equal? d1 d2))) (* 2 d3)]
    [(and (= d2 d3) (not (equal? d1 d4))) (* 2 d3)]))