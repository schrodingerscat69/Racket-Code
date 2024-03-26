;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a05q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A ChessResult is a (anyof 'white-wins 'black-wins 'draw)

;; A ChessMatch is a (list Str Str ChessResult)
;; Requires: white, black are distinct and not empty

;; make-chess-match: Str Str ChessResult -> ChessMatch
(define (make-chess-match white black result)
  (list white black result))
;; chess-match-white: ChessMatch -> Str
(define (chess-match-white match)
  (first match))
;; chess-match-black: ChessMatch -> Str
(define (chess-match-black match)
  (second match))
;; chess-match-result: ChessMatch -> ChessResult
(define (chess-match-result match)
  (third match))

(define (score? player matches)
  (cond
    [(and (equal? player (chess-match-white matches))
          (equal? (chess-match-result matches) 'draw)) 0.5]
    [(and (equal? player (chess-match-black matches))
          (equal? (chess-match-result matches) 'draw)) 0.5]
    [(and (equal? player (chess-match-white matches))
          (equal? (chess-match-result matches) 'white-wins)) 1]
    [(and (equal? player (chess-match-black matches))
          (equal? (chess-match-result matches) 'black-wins)) 1]
    [else 0]))
  

(define (score-per-player player matches)
  (cond
    [(empty? matches) empty]
    [else (cons
           (score? player (first matches))
           (score-per-player player (rest matches)))]))

(define (total-score lst)
  (cond
    [(empty? lst) 0]
    [else (+ (first lst)
             (total-score (rest lst)))]))

(define (chess-score player matches)
  (total-score (score-per-player player matches)))
  
(define world-championship-2008
  (list (make-chess-match "Kramnik" "Anand" 'draw)
        (make-chess-match "Anand" "Kramnik" 'draw)
        (make-chess-match "Kramnik" "Anand" 'black-wins)
        (make-chess-match "Anand" "Kramnik" 'draw)
        (make-chess-match "Kramink" "Anand" 'black-wins)
        (make-chess-match "Anand" "Kramnik" 'white-wins)
        (make-chess-match "Anand" "Kramnik" 'draw)
        (make-chess-match "Kramnik" "Anand" 'draw)
        (make-chess-match "Anand" "Kramnik" 'draw)
        (make-chess-match "Kramnik" "Anand" 'white-wins)
        (make-chess-match "Anand" "Kramnik" 'draw)))

(chess-score "Anand" world-championship-2008)
(chess-score "Kramnik" world-championship-2008)
(chess-score "Kasparov" world-championship-2008) 