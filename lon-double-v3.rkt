;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lon-double-v3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

;; A list of numbers, lon, is either:
;;  1. empty
;;  2. (cons number lon)

;; Sample lon
(define ELON '())
(define LON1 '(2 3 4 5))
(define LON2 '(8 -2 0))

#|
Template for functions on a lon

;; lon ... --> ...
;; Purpose:
(define (f-on-lon a-lon ...)
  (if (empty? a-lon)
      ...
      ...(first a-lon)...(f-on-lon (rest a-lon))))

(check-expect (f-on-lon '() ...) ...)
(check-expect (f-on-lon ... ...) ...)
 ...
(check-expect (f-on-lon ... ...) ...)
|#

;; lon --> lon
;; Purpose: Double the numbers in the given list of numbers
(define (lon-double a-lon)
  (for/list ([a-num 

;; Sample expressions for lon-double
(define DOUBLE-ELON '())
(define DOUBLE-LON1 (cons (* 2 (first LON1)) (lon-double (rest LON1))))
(define DOUBLE-LON2 (cons (* 2 (first LON2)) (lon-double (rest LON2))))

;; Tests using sample computations
(check-expect (lon-double ELON) DOUBLE-ELON)
(check-expect (lon-double LON1) DOUBLE-LON1)
(check-expect (lon-double LON2) DOUBLE-LON2)

;; Tests using sample values
(check-expect (lon-double '(-8 -10)) '(-16 -20))
(check-expect (lon-double '(23 -850 209)) '(46 -1700 418))