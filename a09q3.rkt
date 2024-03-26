;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a09q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;;*******************************************
;;    Mann Sunil Gandhi (21038332)
;;    CS 115 Fall 2023
;;    Assignment 9, Question 3
;;*******************************************
;;

(define-struct country (code name count))
;; a Country is a (make-country Sym Str Nat)
;; Where:
;;   code is an ISO 3166-1 code for this country
;;   name is the official state name of a country according to ISO 3166-1
;;   count is the number of people from some population for which this country 
;;     is their home country

(define-struct bucket (count locs))
;; a Bucket is a (make-bucket Nat (listof Sym))
;; Where:
;;   each entry in locs is a distinct ISO 3166-1 country code

;; (histogram countries names) produces a bucket organizing each county from
;;  countries if found in the list names, with their count.
;; histogram: (listof County) (listof Str) -> (listof Bucket)

(define (histogram countries names)
  (local
    [(define (in-list? countries names)
       (cond
         [(empty? countries) empty]  
         [else 
          (local
            ;; Constants related to proccesing the list of countries.
            [(define current-country (first countries))
             (define rest-countries (rest countries))
             (define country-name-in-names?
               (member (country-name current-country) names))]
            (cond
              [country-name-in-names?
               (cons (make-bucket (country-count current-country)
                                  (list (country-code current-country)))
                     (in-list? rest-countries names))]
              [else (in-list? rest-countries names)]))]))
     
     (define (sorting-buckets lst)
       (sort lst (lambda (bucket1 bucket2)
                   (< (bucket-count bucket1) (bucket-count bucket2)))))
     
     (define (merge-buckets bucket-list)
       (local
         [(define (merge-locs buckets)
            (foldr append empty (map bucket-locs buckets)))

          (define (remove-duplicates lst)
            (foldr (lambda (item acc)
                     (cond
                       [(member item acc) acc]
                       [else (cons item acc)]))
                   empty lst))

          (define (group-by-count buckets count)
            (filter (lambda (b) (= (bucket-count b) count)) buckets))

          (define unique-counts (remove-duplicates
                                 (map bucket-count bucket-list)))]

         (map (lambda (count)
                (make-bucket count (remove-duplicates
                                    (merge-locs
                                     (group-by-count bucket-list count)))))
              unique-counts)))]
    
    (merge-buckets (sorting-buckets (in-list? countries names)))))

(define cs115countries
  (list 
   (make-country 'ARG "Argentina" 1)
   (make-country 'BGD "Bangladesh" 6)
   (make-country 'BRA "Brazil" 1)
   (make-country 'CAN "Canada" 440)
   (make-country 'CHN "China" 373)
   (make-country 'CMR "Cameroon" 1)
   (make-country 'ECU "Ecuador" 1)
   (make-country 'GBR "United Kingdom GB & NIR" 1)
   (make-country 'HKG "Hong Kong" 1)
   (make-country 'IDN "Indonesia" 1)
   (make-country 'IND "India" 37)
   (make-country 'IRN "Iran (Islamic Republic Of)" 1)
   (make-country 'IRQ "Iraq" 2)
   (make-country 'JPN "Japan" 1)
   (make-country 'KOR "Korea, Republic of" 21)
   (make-country 'LBN "Lebanon" 1)
   (make-country 'MUS "Mauritius" 5)
   (make-country 'NGA "Nigeria" 2)
   (make-country 'PAK "Pakistan" 12)
   (make-country 'PHL "Philippines" 1)
   (make-country 'QAT "Qatar" 1)
   (make-country 'SGP "Singapore" 1)
   (make-country 'SYR "Syrian Arab Republic" 1)
   (make-country 'THA "Thailand" 1)
   (make-country 'TTO "Trinidad and Tobago" 13)
   (make-country 'TWN "Taiwan" 2)
   (make-country 'UKR "Ukraine" 2)
   (make-country 'USA "United States" 6)
   (make-country 'VEN "Venezuela" 2)
   (make-country 'VNM "Viet Nam" 5)))
