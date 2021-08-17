;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname triangle-area) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Sample expressions for the area of a triangle

(define AREA-T1 (local [(define SP (/ (+ 3 4 5) 2))]
                  (sqrt (* SP (- SP 3) (- SP 4) (- SP 5)))))

(define AREA-T2 (local [(define SP (/ (+ 2 (sqrt 10) (sqrt 10)) 2))]
                  (sqrt (* SP (- SP (sqrt 10)) (- SP 2) (- SP (sqrt 10))))))

;; A positive number, number>0, is a number greater than 0.

;; number>0 number>0 number>0  --> number>0
;; Purpose: Compute the area of the triangle from the given lengths
(define (triangle-area s1 s2 s3)
  (local [(define SP (/ (+ s1 s2 s3) 2))]
    (sqrt (* SP (- SP s1) (- SP s2) (- SP s3)))))

;; Tests using sample computations
(check-within (triangle-area 3 4 5) AREA-T1 0.01)
(check-within (triangle-area 2 (sqrt 10) (sqrt 10)) AREA-T2 0.01)

;; Tests using sample values
(check-within (triangle-area (sqrt 5) 2 (sqrt 5)) 2 0.01)
(check-within (triangle-area 1 2 (sqrt 5)) 1 0.01)
