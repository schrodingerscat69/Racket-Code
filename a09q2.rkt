;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    Mann Sunil Gandhi (21038332)
;;    CS 115 Fall 2023
;;    Assignment 9, Question 2
;;*******************************************
;;

(define-struct team (name wins draws losses goal-difference))
;; A Team is a (make-team Str Nat Nat Nat Int)
;; Where:
;;   name is the name of the team
;;   wins is the number of games won by the team
;;   draws is the number of games tied by the team
;;   losses is the number of games lost by the team
;;   goal-difference is the number of goals scored by the team minus
;;                      the number of goals scored against the team

;; (league-winner teams) produces the name of the winning team in list of teams.
;; league-winner: (listof Team) -> Str
;; Requires:
;;  * No teams have both an equal number of points and an equal goal difference

;; Examples:
(check-expect (league-winner empty) empty)
(check-expect (league-winner (list (make-team "Barcelona" 30 0 0 75)
                                   (make-team "Atletico Madrid" 29 5 6 74)))
              "Atletico Madrid")
(check-expect (league-winner (list (make-team "Barcelona" 30 7 0 75)
                                   (make-team "Atletico Madrid" 29 5 6 75)))
              "Barcelona")

(define (league-winner teams)
  (local
    [;; (points team) produces the total points won by the team.
     ;; points: Team -> Nat
     
     (define (points team)
       (+ (* 3 (team-wins team)) (team-draws team)))
     
     ;; (winner? current-team team) produces the team that has the most points
     ;;  between the cuurent-team and the next team team. If two teams have the
     ;;  same points, it produces the team that has a higher goal-difference.
     ;; winner?: Team Team -> Team
     
     (define (winner? current-team team)
       (cond
         [(> (points team) (points current-team)) team]
         [(< (points team) (points current-team)) current-team]
         [(> (team-goal-difference team) (team-goal-difference current-team))
          team]
         [else current-team]))]

    (cond
      [(empty? teams) empty]
      [else (team-name (foldr winner? (first teams) (rest teams)))])))

;; Sample teams
(define mci (make-team "Manchester City" 29 6 3 73))
(define liv (make-team "Liverpool" 28 8 2 68)) 
(define che (make-team "Chelsea" 21 11 6 43))
(define tot (make-team "Tottenham" 22 5 11 29))
(define psg (make-team "PSG" 29 6 1 75))
(define bvb (make-team "Borussia Dortmund" 25 4 3 65))
(define acm (make-team "AC Milan" 20 7 5 41))
(define rmd (make-team "Real Madrid" 28 8 4 69))

;; Tests:
(check-expect (league-winner (list mci liv che tot)) "Manchester City")
(check-expect (league-winner (list tot che liv mci)) "Manchester City")
(check-expect (league-winner (list acm mci liv tot che psg bvb)) "PSG")
(check-expect (league-winner (list acm)) "AC Milan")
