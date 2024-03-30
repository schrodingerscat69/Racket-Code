;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a07q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    CS 115 Fall 2023
;;    Assignment 7, Question 3
;;*******************************************
;;

;; A Pair is a (list Any Any)

;; Constructor
(define (make-pair key val) (list key val))
;; Selectors
(define (pair-key p) (first p))
(define (pair-val p) (second p))

;; An AssociationList (AL) is a (listof Pair)

;; A TableChart is an AL.
;; Requires:
;;   * Each key in the AL is a Nat.
;;   * Each value in the AL is a non-empty (listof Str).
;;   * No key occurs twice.
;;   * No string occurs twice.

;; (decreasing-tables n) produces n tables decreasing in size from n to 1
;; decreasing-tables: Nat -> TableChart

;; Examples
(check-expect (decreasing-tables 0) empty)
(check-expect (decreasing-tables 3)
              (list 
               (make-pair 3 (list "X" "X" "X"))
               (make-pair 2 (list "X" "X"))
               (make-pair 1 (list "X"))))

(define (decreasing-tables n)
  (cond
    [(= n 0) empty]
    [else (cons (make-pair n (make-list n "X")) (decreasing-tables (sub1 n)))]))

  
;; (increasing-tables n) produces n tables increasing in size from 1 to n
;; increasing-tables: Nat -> TableChart

;; Examples
(check-expect (increasing-tables 0) empty)
(check-expect (increasing-tables 3)
              (list (make-pair 1 (list "X"))
                    (make-pair 2 (list "X" "X"))
                    (make-pair 3 (list "X" "X" "X"))))

(define (increasing-tables n)
  (local
    [;; (increasing-tables-helper m n) produces tables increasing in size
     ;; from m to n (inclusive)
     ;; increasing-tables-helper: Nat -> TableChart
     (define (increasing-tables-helper m n)
       (cond
         [(> m n) empty]
         [else (cons (make-pair m (make-list m "X"))
                     (increasing-tables-helper (add1 m) n))]))]

    (increasing-tables-helper 1 n)))

;; (num-of-biggest-table tables) produces the table number of the table with
;;  the largest in tables. If the length of two of more tables are equal, it
;;  produces the largest table number.
;; num-of-biggest-table: TableChart -> Nat

;; Examples:
(check-expect (num-of-biggest-table
               (list (make-pair 1 (list "Jaden" "Taylor" "Anthony")))) 1)
(check-expect (num-of-biggest-table
               (list (make-pair 1 (list "A" "B" "C" "D"))
                     (make-pair 2 (list "E" "F" "G" "H"))
                     (make-pair 3 (list "I" "J" "K" "L")))) 3)
                     

(define (num-of-biggest-table tables)
  (local
    [;; (length-of-table tables) produces the length of each table in tables.
     ;; length-of-table: TableChart -> Nat
     
     (define (length-of-table tables)
       (length (pair-val tables)))

     ;; (table-number tables) produces the table number of each table in tables.
     ;; table-number: TableChart -> Nat
     
     (define (table-number tables)
       (pair-key tables))

     ;; (largest-group tables) produces a list which describes the largest
     ;;  table in tables.
     ;; largest-group: TableChart -> (listof Nat (listof Str))
     
     (define (largest-group tables)
       (cond
         [(empty? (rest tables)) (first tables)]
         [else
          (local
            [;; Constant to hold the value of the current largest table in
             ;;  tables. 
             (define current-largest (largest-group (rest tables)))]
            (cond
              [(> (length-of-table (first tables))
                  (length-of-table current-largest))
               (first tables)]
              [(and (equal? (length-of-table (first tables))
                            (length-of-table current-largest))
                    (> (table-number (first tables))
                       (table-number current-largest)))
               (first tables)]
              [else current-largest]))]))]
    (table-number (largest-group tables))))

(define cs115-banquet-tables
  (list (make-pair 5 (list "Troy" "Nichole" "Barbara" "Yifei"))
        (make-pair 9 (list "Lori" "Yuchen" "Karina"))
        (make-pair 8 (list "Joseph" "J.P." "William" "Alexis"))))

(define another-table
  (list (make-pair 47 (list "Arjun" "Ridham" "Jason" "Kara" "Clara"))
        (make-pair 56 (list "Jake" "Amy" "Holt"))
        (make-pair 11 (list "Charles" "Gina" "Terry" "Rosa"))))

(define third-table
  (list (make-pair 6 (list "Anderson" "Smith"))
        (make-pair 9 (list "June" "July" "August"))))

(define fourth-table
  (list (make-pair 7 (list "May" "June" "December" "April"))
        (make-pair 6 (list "Mary" "Peter" "Harry" "MJ"))))

;; Tests:
(check-expect (num-of-biggest-table cs115-banquet-tables) 8)
(check-expect (num-of-biggest-table (decreasing-tables 1000)) 1000)
(check-expect (num-of-biggest-table (decreasing-tables 100)) 100)
(check-expect (num-of-biggest-table (increasing-tables 1000)) 1000)
(check-expect (num-of-biggest-table (increasing-tables 100)) 100)
(check-expect (num-of-biggest-table another-table) 47)
(check-expect (num-of-biggest-table third-table) 9)
(check-expect (num-of-biggest-table fourth-table) 7)