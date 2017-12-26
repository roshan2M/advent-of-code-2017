;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Day 1 - Inverse Captcha|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ***********************
;; Advent of Code 2017
;; Day 1 - Inverse Captcha
;; Roshan Munjal
;; ***********************

;; Part I

;; (sum-digits n) finds the sum of consecutive digits in n that
;;   are equal in value (given that the list is circular, so we
;;   check the first and last elements too)
;; sum-consec: Nat -> Nat
(define (sum-digits n)
  (local [(define digits (nat->lon n))]
    (sum-digits/lon (append digits (list (first digits))))))

;; (nat->lon n acc) converts a natural number n to a list of digits
;; nat->lon: Nat -> (listof Nat)
(define (nat->lon n)
  (local [(define quot (quotient n 10))]
    (cond [(= quot 0) (list n)]
          [else (append (nat->lon quot)
                        (list (remainder n 10)))])))

;; (sum-digits/lon lon) produces the sum of consecutive numbers
;;   in lon that are equal in value
;; sum-digits/lon: (listof Num) -> Num
(define (sum-digits/lon lon)
  (cond [(or (empty? lon)
             (empty? (rest lon))) 0]
        [(= (first lon) (second lon))
         (+ (first lon) (sum-digits/lon (rest lon)))]
        [else (sum-digits/lon (rest lon))]))

;; Part II

;; (sum-pairs n) produces the sum of pairs of digits of n, adding the
;;   kth digit to the (k+n/2)th digit <=> the kth digit equals the
;;   (k+n/2)th digit in n
;; sum-pairs: Nat -> Nat
(define (sum-pairs n)
  (local [(define lst (nat->lon n))]
    (sum-pairs/lst (first-part (/ (length lst) 2) lst) (rest-part (/ (length lst) 2) lst))))

;; (sum-pairs/lst lst1 lst2) produces the element wise sum of sum1
;;   and lst2 only if the first elements of each lists are equal
;; sum-pairs/lst: (listof Nat) (listof Nat) -> Nat
(define (sum-pairs/lst lst1 lst2)
  (cond [(empty? lst1) 0]
        [(= (first lst1) (first lst2))
         (+ (* 2 (first lst1)) (sum-pairs/lst (rest lst1) (rest lst2)))]
        [else (sum-pairs/lst (rest lst1) (rest lst2))]))

;; (first-part n lst) returns a list of the first n elements in lst
;; split-list: Nat (listof Nat) (listof Nat) -> (list (listof Nat) (listof Nat))
(define (first-part n lst)
  (cond [(= n 0) empty]
        [else (cons (first lst) (first-part (sub1 n) (rest lst)))]))

;; (rest-part n lst) produces a list of all elements in lst after the
;;   first n elements
;; rest-part: Nat (listof Nat) -> (listof Nat)
(define (rest-part n lst)
  (cond [(= n 0) lst]
        [else (rest-part (sub1 n) (rest lst))]))
