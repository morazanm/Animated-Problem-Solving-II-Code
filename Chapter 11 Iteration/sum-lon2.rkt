;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sum-lon2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define ELON '())
(define LON1 '(6 4 9 4 2))
(define LON2 '(-10 0 7 -20))

;; (listof number) --> number
;; Purpose: Add the numbers in the given list of numbers
(define (sumr-lon a-lon)
  (if (empty? a-lon)
      0
      (local [(define sum-rest (sumr-lon (rest a-lon)))]
        (+ (first a-lon) sum-rest))))

(check-expect (sum-lon4 '()) 0)
(check-expect (sum-lon4 '(1 2 3)) 6)
(check-expect (sum-lon4 '(7 31 8)) 46)

;; (listof number) number --> number
;; Purpose: Add the numbers in the given lon
;; Accumulator invariant
;;  accum = sum of list elements so far
(define (sum-lon-accum a-lon accum)
  (if (empty? a-lon)
      accum
      (sum-lon-accum
       (rest a-lon)
       (+ (first a-lon) accum))))

;; Sample expressions for sum-lon-accum
(define SUMACCUM-ELON1 0)
(define SUMACCUM-ELON2 20)
(define SUMACCUM-LON1 (sum-lon-accum
                       (rest LON1)
                       (+ (first LON1) 0)))
(define SUMACCUM-RLON2 (sum-lon-accum
                        (rest (rest LON2))
                        (+ (first (rest LON2)) (first LON2))))

;; Tests using sample computations for sum-lon-accum
(check-expect (sum-lon-accum ELON 0)  SUMACCUM-ELON1)
(check-expect (sum-lon-accum ELON 20) SUMACCUM-ELON2)
(check-expect (sum-lon-accum LON1 0)  SUMACCUM-LON1)
(check-expect (sum-lon-accum (rest LON2) (first LON2))
              SUMACCUM-RLON2)

;; Tests using sample values for sum-lon-accum
(check-expect (sum-lon-accum '(1 2 3 4)  0) 10)
(check-expect (sum-lon-accum '(7 31 8)   0) 46)




;; (listof number) --> number
;; Purpose: Add the numbers in the given list of numbers
(define (sum-lon4 a-lon)
  (sum-lon-accum (reverse a-lon) 0))

(check-expect (sum-lon4 '()) 0)
(check-expect (sum-lon4 '(1 2 3)) 6)
(check-expect (sum-lon4 '(7 31 8)) 46)

;; (listof number) --> number
;; Purpose: Add the numbers in the given list of numbers
(define (sum-lon5 a-lon) (foldr + 0 a-lon))

(check-expect (sum-lon5 '()) 0)
(check-expect (sum-lon5 '(1 2 3)) 6)
(check-expect (sum-lon5 '(7 31 8)) 46)

(define L (build-list 1000000 (λ (i) (random 100000))))

(define T4 (time (sum-lon4 L)))
(define T5 (time (sum-lon5 L)))

