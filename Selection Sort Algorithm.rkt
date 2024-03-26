;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Selection Sort Algorithm|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Find the minimum element in a list
(define (find-min lst)
  (cond
    [(empty? lst) empty]
    [(empty? (rest lst)) (first lst)]
    [(< (first lst) (find-min (rest lst))) (first lst)]
    [else (find-min (rest lst))]))

; Remove the first occurrence of an element from a list
(define (remove-first occurrence lst)
  (cond
    [(empty? lst) empty]
    [(equal? occurrence (first lst)) (rest lst)]
    [else (cons (first lst) (remove-first occurrence (rest lst)))]))

; Selection sort implementation
(define (selection-sort lst)
  (cond
    [(empty? lst) empty]
    [else (cons (find-min lst) 
                (selection-sort (remove-first (find-min lst) lst)))]))

; Example usage
(selection-sort empty)
(selection-sort (list 1 2 3))
(selection-sort (list  3 8 9))
(selection-sort (list 5 4 3 2 1))
(selection-sort (list 5 3 6 2 10))
(selection-sort (list 6 9 3 1 2 0))


