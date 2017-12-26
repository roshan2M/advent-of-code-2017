;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Day 2 - Corruption Checksum|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***************************
;; Advent of Code 2017
;; Day 2 - Corruption Checksum
;; Roshan Munjal
;; ***************************

;; Part I

;; (checksum lolon) consumes a lolon (listof (listof Num))
;;   and computes the highest difference in each row and
;;   returns the sum over all rows
;; checksum: (listof (listof Num)) -> Num
;; requires: each list in lolon is non-empty
(define (checksum lolon)
  (cond [(empty? lolon) 0]
        [else (+ (highest-diff (first lolon) (first (first lolon))
                               (first (first lolon)))
                 (checksum (rest lolon)))]))

;; (highest-diff row low high) produces the highest difference
;;   in row given the current low and high
;; highest-diff: (listof Num) Num Num -> 
(define (highest-diff row low high)
  (cond [(empty? row) (- high low)]
        [(< (first row) low)
         (highest-diff (rest row) (first row) high)]
        [(> (first row) high)
         (highest-diff (rest row) low (first row))]
        [else (highest-diff (rest row) low high)]))

;; Part II

;; (find-sum-fm lolon) finds the sum of even multiples and divisors
;;   in each list of naturals in lolon and adds them together
;; find-sum-fm: (listof (listof Nat)) -> Nat
;; requires: each list must have an evenly divisible pair of naturals
(define (find-sum-fm lolon)
  (cond [(empty? lolon) 0]
        [else (+ (find-sum-fm/lst (first lolon))
                 (find-sum-fm (rest lolon)))]))

;; (find-sum-fm/lst lon) finds an evenly divisible pair in lon
;;   and returns the result of its integer division, false otherwise
;; find-sum-fm/lst: (listof Nat) -> (anyof Nat false)
;; requires: each list must have an evenly divisible pair
(define (find-sum-fm/lst lon)
  (cond [(empty? lon) false]
        [else (local [(define factor (search-factor (first lon) (rest lon)))
                      (define multiple (search-multiple (first lon) (rest lon)))]
                (cond [(integer? factor) factor]
                      [(integer? multiple) multiple]
                      [else (find-sum-fm/lst (rest lon))]))]))

;; (search-factor current lon) searches for a factor of current
;;   in lon, returns the integer division if a factor exists,
;;   false otherwise
;; search-factor: Nat (listof Nat) -> (anyof Nat false)
(define (search-factor current lon)
  (cond [(empty? lon) false]
        [(integer? (/ current (first lon))) (/ current (first lon))]
        [else (search-factor current (rest lon))]))

;; (search-multiple current lon) searches for a multiple of current
;;   in lon, returns the integer devision if a multiple exists,
;;   false otherwise
;; search-multiple: Nat (listof Nat) -> (anyof Nat false)
(define (search-multiple current lon)
  (cond [(empty? lon) false]
        [(integer? (/ (first lon) current)) (/ (first lon) current)]
        [else (search-multiple current (rest lon))]))
