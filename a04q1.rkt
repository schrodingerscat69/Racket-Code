;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a04q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    CS 115 Fall 2023
;;    Assignment 4, Question 1
;;*******************************************
;;

;; (count-sym symbol lst) produces the number of occurences of symbol
;;  present in the list lst.
;; count-sym: Sym (listof Sym) -> Nat
;; Requires:
;;  * sym can only be 'left or 'right
;;  * lst to be a list of only the symbols 'left and/or 'right, or empty

;; Examples:
(check-expect (count-sym 'right empty) 0)
(check-expect (count-sym 'left (cons 'left
                                     (cons 'right
                                           (cons 'left empty)))) 2)
(check-expect (count-sym 'right
                         (cons 'right
                               (cons 'left
                                     (cons 'left
                                           (cons 'left
                                                 (cons 'right empty)))))) 2)

(define (count-sym symbol lst)
  (cond
    [(empty? lst) 0]
    [(equal? (first lst) symbol)
     (add1 (count-sym symbol (rest lst)))]
    [else (count-sym symbol (rest lst))]))

;; (list-of-symbols sym count) produces a list of length count
;;  with each element being sym.
;; list-of-symbols: Sym Nat -> (listof Sym)
;; Requires:
;;  * sym can only be 'left or 'right

;; Examples:
(check-expect (list-of-symbols 'left 0) empty)
(check-expect (list-of-symbols 'right 4)
              (cons 'right
                    (cons 'right
                          (cons 'right
                                (cons 'right empty)))))
         
(define (list-of-symbols sym count)
  (cond
    [(zero? count) empty]
    [else (cons sym (list-of-symbols sym (sub1 count)))]))

;; (reduce-movements lon) produces a list of 'right symbols, if the number of 
;;  occurences of 'right is greater than the number of occurences of 'left 
;;  with the length of the returned list being the difference between the 
;;  number of occurences of 'right and 'left; produces a list of 'left symbols, 
;;  if the number of occurences of 'left is greater than the number of  
;;  occurences of 'right with the length of the returned list being the   
;;  difference between the occurences of 'left and 'right; produces empty if   
;;  the number of occurences of 'right and 'left are equal.
;; reduce-movements: (listof (anyof 'right 'left)) ->
;;                   (listof (anyof 'right 'left))
;; Requires:
;;  * lon to be a list of only the symbols 'left and/or 'right, or empty

;; Examples:
(check-expect (reduce-movements (cons 'right
                                      (cons 'right
                                            (cons 'left
                                                  (cons 'left empty)))))
              empty)
(check-expect (reduce-movements (cons 'left
                         (cons 'right
                               (cons 'left
                                     (cons 'left
                                           (cons 'right
                                                 (cons 'left empty)))))))
              (cons 'left (cons 'left empty)))

(define (reduce-movements lon)
  (cond
    [(= (count-sym 'left lon) (count-sym 'right lon))
     empty]
    [(> (count-sym 'left lon) (count-sym 'right lon))
     (list-of-symbols 'left
                      (- (count-sym 'left lon)
                         (count-sym 'right lon)))]
    [else (list-of-symbols 'right
                           (- (count-sym 'right lon)
                              (count-sym 'left lon)))]))

;; Tests:
(check-expect (reduce-movements empty) empty)
(check-expect (reduce-movements (cons 'left
                                      (cons 'right
                                            (cons 'left
                                                  (cons 'right empty)))))
              empty)
(check-expect (reduce-movements (cons 'left
                                      (cons 'right
                                            (cons 'left empty))))
              (cons 'left empty))
(check-expect (reduce-movements (cons 'left
                                      (cons 'right
                                            (cons 'right
                                                  (cons 'right empty)))))
              (cons 'right (cons 'right empty)))