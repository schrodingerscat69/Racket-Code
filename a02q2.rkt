;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a02q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (over-speed? speed limit)
  (or (>= speed 150)
      (and (>= speed 80) (<= limit 80) (>= (- speed limit) 40))
      (>= (- speed limit) 50)))

(define (speed-diff speed limit) (- speed limit))

(define (fine-km speed limit)
  (cond [(>= (speed-diff speed limit) 50) (* 9.75 (speed-diff speed limit))]
        [(>= (speed-diff speed limit) 30) (* 7 (speed-diff speed limit))]
        [(>= (speed-diff speed limit) 20) (* 4.5 (speed-diff speed limit))]
        [(>= (speed-diff speed limit) 10) (* 3 (speed-diff speed limit))]
        [else 0]))

(define (speeding-fine speed limit)
    (cond [(over-speed? speed limit) (+ 10000 (fine-km speed limit))]
        [else (fine-km speed limit)]))
