;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fold-from-right) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;; (listof number) --> number

(define (sum-lon5 a-lon)
  (local [(define (sum-lon-accum a-lon accum)
            (if (empty? a-lon)
                accum
                (sum-lon-accum
                 (rest a-lon)
                 (+ (first a-lon) accum))))]
    (sum-lon-accum (reverse a-lon) 0)))

(check-expect (sum-lon5 '()) 0)
(check-expect (sum-lon5 '(1 2 3)) 6)
(check-expect (sum-lon5 '(7 31 8)) 46)


;; (listof string) --> (listof natnum)

(define (lengths-lostr2 a-lostr)
  (local [(define (lengths-lostr-accum a-lostr accum)
            (if (empty? a-lostr)
                accum
                (lengths-lostr-accum
                 (rest a-lostr)
                 (cons (string-length (first a-lostr)) accum))))]
    (lengths-lostr-accum (reverse a-lostr) '())))

(check-expect (lengths-lostr2 '()) '())
(check-expect (lengths-lostr2 '("a" "b" "c")) '(1 1 1))
(check-expect (lengths-lostr2 '("art" "be" "cons")) '(3 2 4))

;; Y (X Y --> Y) (listof X) --> Y
;; Purpose: Fold the given list from right to left using the given
;;          initial accumulator value and given combinator function.
(define (fold-from-right base comb a-lox)
  (local [;; (listof X) Y --> Y
          ;; Purpose: Fold the list values from right to left into
          ;;          the given accumulator.
          ;; Accumulator invariant:
          ;;   accum = Y value for traversed list elements so far
          (define (aux-f a-lox accum)
            (if (empty? a-lox)
                accum
                (aux-f
                 (rest a-lox)
                 (comb (first a-lox) accum))))]
    (aux-f (reverse a-lox) base)))

(define (sum-lon6 a-lon)
  (fold-from-right 0 + a-lon))

(check-expect (sum-lon6 '()) 0)
(check-expect (sum-lon6 '(1 2 3)) 6)
(check-expect (sum-lon6 '(7 31 8)) 46)

(define (lengths-lostr4 a-lostr)
  (fold-from-right '() (λ (str accum) (cons (string-length str) accum)) a-lostr))

(check-expect (lengths-lostr4 '()) '())
(check-expect (lengths-lostr4 '("a" "b" "c")) '(1 1 1))
(check-expect (lengths-lostr4 '("art" "be" "cons")) '(3 2 4))