;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Day 6 - Memory Reallocation|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ***************************
;; Advent of Code 2017
;; Day 6 - Memory Reallocation
;; Roshan Munjal
;; ***************************

;; PART I

;; (n-allocations lon) finds how many times memory
;;   must be reallocated before cycling back to the
;;   same allocation
;; n-allocations: (listof Nat) -> Nat
(define (n-allocations lon)
  (n-allocations/acc lon empty))

;; (n-allocations/acc lon acc) finds how many times memory
;;   must be reallocated before it cycles by checking
;;   if the current memory system belongs to the acc
(define (n-allocations/acc lon acc)
  (cond [(member? lon acc) 0]
        [else (add1 (n-allocations/acc (reallocate lon) (cons lon acc)))]))

;; (reallocate memory) reallocates the numbers in memory
;;   based on the given rule
;; reallocate: (listof Nat) -> (listof Nat)
(define (reallocate memory)
  (local [(define result (maxlist memory 0 (first memory)))
          (define maximum (first result))
          (define index (second result))]
    (list-ref memory index)))

;; (maxlist lon) produces the maximum element in lon
;; maxlist: (listof Num) -> Num
(define (maxlist lon index current)
  (cond [(empty? (rest lon)) (list index current)]
        [else (maxlist (rest lon) (add1 index) (max (first lon) current))]))