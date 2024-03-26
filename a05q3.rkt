;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a05q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; A SeatingList is an AL.
;; Requires:
;;   * Each key in the AL is a Str.
;;   * Each value in the AL is a Nat.
;;   * The keys are in sorted order.
;;   * No key occurs twice.

(define cs115-banquet-tables
  (list (make-pair 5 (list "Troy" "Nichole"))
        (make-pair 3 (list "Lori" "Yuchen" "Karina"))
        (make-pair 2 (list "Joseph" "J.P." "William" "Alexis" "Yifei"))))

(define (insert-in-order new-pair sorted-acc)
  (cond
    [(empty? sorted-acc) (list new-pair)]
    [(string<? (pair-key new-pair) (pair-key (first sorted-acc)))
     (cons new-pair sorted-acc)]
    [else
     (cons (first sorted-acc)
           (insert-in-order new-pair (rest sorted-acc)))]))

(define (my-sort lst acc)
  (cond
    [(empty? lst) acc]
    [else
     (my-sort (rest lst)
              (insert-in-order (first lst) acc))]))

;; pair is of form (make-pair Nat (list Str))


(define (flatten lst currval currlst acc)
  (cond
    [(empty? lst) acc]
    [(empty? (rest currlst))
     (cond [(empty? (rest lst)) (cons (make-pair (first currlst) currval) acc)]
           [else (flatten (rest lst) (pair-key (first (rest lst)))
                          (pair-val (first (rest lst)))
                          (cons (make-pair (first currlst) currval) acc))])]
    [else (flatten lst (pair-key (first lst))
                       (rest currlst)
                       (cons (make-pair (first currlst) currval) acc))]))

(define (seating-list tables)
  (cond [(empty? tables) empty]
        [else (my-sort (flatten tables
                                (pair-key (first tables))
                                (pair-val (first tables))
                                empty) empty)]))