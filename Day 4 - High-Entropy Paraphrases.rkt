;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Day 4 - High-Entropy Paraphrases|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ********************************
;; Advent of Code 2017
;; Day 4 - High Entropy Passphrases
;; Roshan Munjal
;; ********************************

;; PART I & II

;; (valid-passphrase/lst lop) produces the number of
;;   valid passphrases in lop
;; valid-passphrases/lst: (listof (listof Char)) -> Nat
(define (valid-passphrase/lst lop)
  (cond [(empty? lop) 0]
        ;; Change valid-passphrase1 -> valid-passphrase2 to solve Part II
        [(valid-passphrase1 (tokenize (lambda (k) (char=? k #\space)) (first lop)))
         (add1 (valid-passphrase/lst (rest lop)))]
        [else (valid-passphrase/lst (rest lop))]))

;; (valid-passphrase1 phrase) produces true if phrase
;;   is a valid passphrase, false otherwise
;; valid-passphrase1: (listof (listof Char)) -> Bool
(define (valid-passphrase1 phrase)
  (cond [(empty? phrase) true]
        [(member? (first phrase) (rest phrase)) false]
        [else (valid-passphrase1 (rest phrase))]))

;; (valid-passphrase2 phrase) produces true if phrase
;;   does not contain any anagrams, false otherwise
;; valid-passphrase2: (listof (listof Char)) -> Bool
(define (valid-passphrase2 phrase)
  (cond [(empty? phrase) true]
        [(anagram/lst? (first phrase) (rest phrase)) false]
        [else (valid-passphrase2 (rest phrase))]))

;; (anagram/lst? exp exp/lst) produces true if exp is
;;   an anagram of any expression in exp/lst, false otherwise
;; anagram/lst?: (listof Char) (listof (listof Char)) -> Bool
(define (anagram/lst? exp exp/lst)
  (cond [(empty? exp/lst) false]
        [(anagram? exp (first exp/lst)) true]
        [else (anagram/lst? exp (rest exp/lst))]))

;; (anagram? exp1 exp2) determines if exp1 is an anagram of
;;   exp2, false otherwise
;; anagram?: (listof Char) (listof Char) -> Bool
(define (anagram? exp1 exp2)
  (cond [(and (empty? exp1) (empty? exp2)) true]
        [(or (empty? exp1) (empty? exp2)) false]
        [(member? (first exp1) exp2)
         (anagram? (rest exp1) (remove-char (first exp1) exp2))]
        [else false]))

;; (remove-char c loc) removes the first instance of c in loc
;; remove-char: Char (listof Char) -> (listof Char)
(define (remove-char c loc)
  (cond [(empty? loc) empty]
        [(char=? c (first loc)) (rest loc)]
        [else (cons (first loc) (remove-char c (rest loc)))]))

;; Tokenizing Strings

;; (tokenize break? str) splits a list of characters into a list of
;;   list of characters based on the break? predicate
;; tokenize: (Char -> Bool) (listof Char) -> (listof Str)
(define (tokenize break? loc)
  (cond [(empty? loc) empty]
        [else (cons (first-part break? loc)
                    (tokenize break? (rest-part break? loc)))]))

;; (first-part break? loc) produces all characters before the first
;;   break? in loc
;; first-part: (Char -> Bool) (listof Char) -> (listof Char)
(define (first-part break? loc)
  (cond [(empty? loc) empty]
        [(break? (first loc)) empty]
        [else (cons (first loc)
                    (first-part break? (rest loc)))]))

;; (rest-part break? loc) produces all the characters after the first
;;   break? in loc
;; rest-part: (Char -> Bool) (listof Char)
(define (rest-part break? loc)
  (cond [(empty? loc) empty]
        [(break? (first loc)) (rest loc)]
        [else (rest-part break? (rest loc))]))