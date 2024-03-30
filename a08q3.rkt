;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a08q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    CS 115 Fall 2023
;;    Assignment 8, Question 3
;;*******************************************
;;

;; A Transaction is a (list (anyof 'buy 'sell) Nat Num)
;; Requires: quantity, price > 0

;; Constructor
(define (make-transaction type quantity price)
  (list type quantity price))
;; Selectors
(define (transaction-type txn)
  (first txn))
(define (transaction-quantity txn)
  (second txn))
(define (transaction-price txn)
  (third txn))

;; A Holding is a (list Nat Num)

;; Constructor
(define (make-holding qty acb)
  (list qty acb))
;; Selectors
(define (holding-qty holding)
  (first holding))
(define (holding-acb holding)
  (second holding))

;; (adjusted-cost-base ledger) produces the adjusted cost base in the stock
;;  after each transaction in ledger is processed. 
;; adjusted-cost-base: (listof Transaction) -> Num

;; Examples:
(check-expect (adjusted-cost-base (list (make-transaction 'buy 100 50)))
              5000)
(check-expect (adjusted-cost-base (list (make-transaction 'sell 150 35)
                                        (make-transaction 'buy 150 30)))
              0)
(check-expect (adjusted-cost-base (list (make-transaction 'sell 150 35)
                                        (make-transaction 'buy 150 30)))
              0)


(define (adjusted-cost-base ledger)
  (holding-acb
   (foldr (lambda (transaction holding)
            (local
              ;; Constants related to processing the ledger.
              [(define current-qty (holding-qty holding))
               (define current-acb (holding-acb holding))
               (define txn-qty (transaction-quantity transaction))
               (define txn-prc (transaction-price transaction))
               (define txn-type (transaction-type transaction))]
              (cond
                [(equal? txn-type 'buy)
                 (make-holding
                  (+ current-qty txn-qty)
                  (+ current-acb (* txn-qty txn-prc)))]
                [(equal? txn-type 'sell)
                 (make-holding
                  (- current-qty txn-qty)
                  (- current-acb
                     (* current-acb (/ txn-qty current-qty))))]
                [else holding])))
          (make-holding 0 0) ledger)))

;; Tests:
(check-expect (adjusted-cost-base (list (make-transaction 'inc-sym 100 50)))
              0) 
(check-expect (adjusted-cost-base empty)
              0)
(check-expect (adjusted-cost-base (list (make-transaction 'sell 150 35)
                                        (make-transaction 'buy 250 48)))
              4800)
(check-expect (adjusted-cost-base (list (make-transaction 'sell 50 40)
                                        (make-transaction 'buy 100 30)
                                        (make-transaction 'buy 150 20)))
              4800)
(check-expect (adjusted-cost-base (list (make-transaction 'buy 100 25)
                                        (make-transaction 'buy 50 30)))
              4000)
(check-expect (adjusted-cost-base (list (make-transaction 'buy 0 0))) 0)
(check-expect (adjusted-cost-base (list (make-transaction 'sell 200 45)
                                        (make-transaction 'sell 500 90)
                                        (make-transaction 'buy 1000 100)))
              30000)