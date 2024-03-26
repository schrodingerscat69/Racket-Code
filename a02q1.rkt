;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a02q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (speeding-fine speed limit)
  (define (excessive-speed? speed limit)
    (or (and (>= speed 150) (<= limit 140))
        (and (>= speed 80) (<= limit 80) (>= (- speed limit) 40))
        (>= (- speed limit) 50)))
  
  (define (fine-per-km-over speed limit)
    (cond ((< speed limit) 0)
          ((and (>= speed 50) (< speed 150)) 9.75)
          ((>= speed 30) 7)
          ((>= speed 20) 4.50)
          ((>= speed 10) 3)
          (else 0)))
  
  (define (calculate-fine speed limit)
    (if (excessive-speed? speed limit)
        (+ 10000 (fine-per-km-over speed limit))
        (fine-per-km-over speed limit)))
  
  (calculate-fine speed limit))
