;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fold-from-left) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define ELON '())
(define LON1 '(6 4 9 4 2))
(define LON2 '(-10 0 7 -20))

(define ELOX '())
(define LOX1 '(d c b a))
(define LOX2 '(99 #true "Hi!"))

;; (listof number) --> number
;; Purpose: Add the numbers in the given lon
(define (sum-lon4 a-lon)
  (local [;; (listof number) number --> number
          ;; Purpose: Add the numbers in the given lon
          ;; Accumulator invariant
          ;;  accum = sum of list elements so far
          (define (sum-lon-accum a-lon accum)
            (if (empty? a-lon)
                accum
                (sum-lon-accum
                 (rest a-lon)
                 (+ (first a-lon) accum))))]
    (sum-lon-accum a-lon 0)))


;; Sample expressions for sum-lon4
(define SUM4-ELON 0)
(define SUM4-LON1 (+ (first LON1)
                     (sum-lon4 (rest LON1))))
(define SUM4-LON2 (+ (first LON2)
                     (sum-lon4 (rest LON2))))

;; Tests using sample computations for sum-lon4
(check-expect (sum-lon4 ELON) SUM4-ELON)
(check-expect (sum-lon4 LON1) SUM4-LON1)
(check-expect (sum-lon4 LON2) SUM4-LON2)

;; Tests using sample values for sum-lon4
(check-expect (sum-lon4 '(1 2 3 4)) 10)
(check-expect (sum-lon4 '(7 31 8)) 46)

(define (rev-lox2 a-lox)
  (local [(define (rev-lox-accum a-lox accum)
            (if (empty? a-lox)
                accum
                (rev-lox-accum
                 (rest a-lox)
                 (cons (first a-lox) accum))))]
    (rev-lox-accum a-lox '())))

(check-expect (rev-lox2 '()) '())
(check-expect (rev-lox2 '(1 2 3)) '(3 2 1))
(check-expect (rev-lox2 '(7 31 8 -2)) '(-2 8 31 7))

;; Differences: base value and combinator function

;; Y (X Y --> Y) (listof X) --> Y
;; Purpose: Fold the given list from left to right using the given
;;          initial accumulator value and given combinator function.
(define (fold-from-left base comb a-lox)
  (local [;; (listof X) Y --> Y
          ;; Purpose: Fold the list values from left to right into
          ;;          the given accumulator.
          ;; Accumulator invariant:
          ;;   accum = Y value for traversed list elements so far
          (define (aux-f a-lox accum)
            (if (empty? a-lox)
                accum
                (aux-f
                 (rest a-lox)
                 (comb (first a-lox) accum))))]
    (aux-f a-lox base)))

;; (listof number) --> number
;; Purpose: Add the numbers in the given lon
(define (sum-lon5 a-lon)
  (fold-from-left 0 + a-lon))

;; Sample expressions for sum-lon5
(define SUM5-ELON (fold-from-left 0 + ELON))
(define SUM5-LON1 (fold-from-left 0 + LON1))
(define SUM5-LON2 (fold-from-left 0 + LON2))

;; Tests using sample computations for sum-lon5
(check-expect (sum-lon5 ELON) SUM5-ELON)
(check-expect (sum-lon5 LON1) SUM5-LON1)
(check-expect (sum-lon5 LON2) SUM5-LON2)

;; Tests using sample values for sum-lon5
(check-expect (sum-lon5 '(1 2 3 4)) 10)
(check-expect (sum-lon5 '(7 31 8))  46)


;; (listof X) --> (listof X)
;; Purpose: Reverse the given list
(define (rev-lox5 a-lox)
  (fold-from-left '() cons a-lox))

;; Sample expressions for sum-lon5
(define REV5-ELON (fold-from-left '() cons ELOX))
(define REV5-LON1 (fold-from-left '() cons LOX1))
(define REV5-LON2 (fold-from-left '() cons LOX2))

;; Tests using sample computations for rev-lox5
(check-expect (rev-lox5 ELOX) REV5-ELON)
(check-expect (rev-lox5 LOX1) REV5-LON1)
(check-expect (rev-lox5 LOX2) REV5-LON2)

;; Tests using sample values for rev-lox2
(check-expect (rev-lox5 '(1 2 3 4 5))
              '(5 4 3 2 1))
(check-expect (rev-lox5 '(red white blue))
              '(blue white red))