;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Day 3 - Memory Spiral|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; *********************
;; Advent of Code 2017
;; Day 3 - Memory Spiral
;; Roshan Munjal
;; *********************

;; (spiral-steps n) counts the number of steps required to
;;   move from n to 1 in a spiral memory system
;; spiral-steps: Nat -> Nat
(define (spiral-steps n)
  (local [(define k (find-closest-odd 1 n))]
    (- k 1 (modulo (abs (- (sqr k) n)) k))))

;; (find-closest-odd n) produces the first odd number whose
;;   square is >= n
;; find-closest-odd: Nat -> Nat
(define (find-closest-odd current n)
  (cond [(>= (sqr current) n) current]
        [else (find-closest-odd (+ 2 current) n)]))