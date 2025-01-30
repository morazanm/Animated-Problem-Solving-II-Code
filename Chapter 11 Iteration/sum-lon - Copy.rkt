;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sum-lon) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define ELON '())
(define LON1 '(6 4 9 4 2))
(define LON2 '(-10 0 7 -20))


;; (listof number) --> number
;; Purpose: Add the numbers in the given lon
(define (sum-lon a-lon)
  (if (empty? a-lon)
      0
      (+ (first a-lon)
         (sum-lon (rest a-lon)))))

;; Sample expressions for sum-lon
(define SUM-ELON 0)
(define SUM-LON1 (+ (first LON1)
                    (sum-lon (rest LON1))))
(define SUM-LON2 (+ (first LON2)
                    (sum-lon (rest LON2))))

;; Tests using sample computations for sum-lon
(check-expect (sum-lon ELON) SUM-ELON)
(check-expect (sum-lon LON1) SUM-LON1)
(check-expect (sum-lon LON2) SUM-LON2)

;; Tests using sample values for sum-lon
(check-expect (sum-lon '(1 2 3 4)) 10)
(check-expect (sum-lon '(7 31 8)) 46)

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
                        (+ (second LON2) (first LON2))))

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
;; Purpose: Add the numbers in the given lon
(define (sum-lon2 a-lon)
  (sum-lon-accum a-lon 0))

;; Sample expressions for sum-lon2
(define SUM2-ELON (sum-lon-accum ELON 0))
(define SUM2-LON1 (sum-lon-accum LON1 0))
(define SUM2-LON2 (sum-lon-accum LON2 0))

;; Tests using sample computations for sum-lon2
(check-expect (sum-lon2 ELON) SUM2-ELON)
(check-expect (sum-lon2 LON1) SUM2-LON1)
(check-expect (sum-lon2 LON2) SUM2-LON2)

;; Tests using sample values for sum-lon2
(check-expect (sum-lon2 '(1 2 3 4)) 10)
(check-expect (sum-lon2 '(7 31 8)) 46)



;; (listof number) --> number
;; Purpose: Add the numbers in the given list of numbers
(define (sum-lon3 a-lon)
  (sum-lon-accum (reverse a-lon) 0))

;; Sample expressions for sum-lon3
(define SUM3-ELON (sum-lon-accum (reverse ELON) 0))
(define SUM3-LON1 (sum-lon-accum (reverse LON1) 0))
(define SUM3-LON2 (sum-lon-accum (reverse LON2) 0))

;; Tests using sample computations for sum-lon3
(check-expect (sum-lon3 ELON) SUM3-ELON)
(check-expect (sum-lon3 LON1) SUM3-LON1)
(check-expect (sum-lon3 LON2) SUM3-LON2)

;; Tests using sample values for sum-lon3
(check-expect (sum-lon3 '(1 2 3 4)) 10)
(check-expect (sum-lon3 '(7 31 8)) 46)


;; (listof number) --> number
;; Purpose: Add the numbers in the given lon
;; Accumulator invariant
;;  accum = sum of list elements so far
(define (sum-lon5 a-lon)
  (foldl + 0 a-lon))

;; Sample expressions for sum-lon5
(define SUM5-ELON (foldl (λ (a-num accum)
                           (+ a-num accum))
                         0
                         ELON))

(define SUM5-LON1 (foldl (λ (a-num accum)
                           (+ a-num accum))
                         0
                         LON1))

(define SUM5-LON2 (foldl (λ (a-num accum)
                           (+ a-num accum))
                         0
                         LON2))

;; Tests using sample computations for sum-lon5
(check-expect (sum-lon5 ELON) SUM5-ELON)
(check-expect (sum-lon5 LON1) SUM5-LON1)
(check-expect (sum-lon5 LON2) SUM5-LON2)

;; Tests using sample values for sum-lon5
(check-expect (sum-lon5 '(1 2 3 4)) 10)
(check-expect (sum-lon5 '(7 31 8)) 46)


;; (listof number) --> number
;; Purpose: Add the numbers in the given lon
;; Accumulator invariant
;;  accum = sum of list elements so far
(define (sum-lon6 a-lon)
  (foldr + 0 a-lon))

;; Sample expressions for sum-lon6
(define SUM6-ELON (foldr (λ (a-num accum)
                           (+ a-num accum))
                         0
                         ELON))

(define SUM6-LON1 (foldr (λ (a-num accum)
                           (+ a-num accum))
                         0
                         LON1))

(define SUM6-LON2 (foldr (λ (a-num accum)
                           (+ a-num accum))
                         0
                         LON2))

;; Tests using sample computations for sum-lon6
(check-expect (sum-lon6 ELON) SUM5-ELON)
(check-expect (sum-lon6 LON1) SUM5-LON1)
(check-expect (sum-lon6 LON2) SUM5-LON2)

;; Tests using sample values for sum-lon6
(check-expect (sum-lon6 '(1 2 3 4)) 10)
(check-expect (sum-lon6 '(7 31 8)) 46)


(define L (build-list 1000000 (λ (i) (random 100000))))

(define T1 (time (sum-lon L)))
(define T2 (time (sum-lon2 L))) ;; left to right
(define T3 (time (sum-lon3 L))) ;; right to left
(define T5 (time (sum-lon5 L))) ;; using foldl
(define T6 (time (sum-lon6 L))) ;; using foldr
