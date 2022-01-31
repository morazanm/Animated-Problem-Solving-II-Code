;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname dot-product) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Template for functions on a (vectorof X)

;;; Sample (vectorof X)
;(define VECTOR0 ...)
;...
;(define VECTORK ...)
;     
;;; f-on-vector: (vector X) ... --> ...
;;; Purpose: 
;(define (f-on-vector V ...)
;  (local 
;    [;; f-on-VINTV1: [int int] ... --> ...
;     ;; Purpose: For the given VINTV, ...
;     (define (f-on-VINTV1 low high)
;       (if (> low high) 
;           ...
;           ...(vector-ref V high)...(f-on-VINTV1 low (sub1 high))))
;
;     ;; f-on-VINTV2: [int int] ... --> ...
;     ;; Purpose: For the given VINTV2, ...
;     (define (f-on-VINTV2 low high)
;       (if (> low high) 
;           ...
;           ...(vector-ref V low)...(f-on-VINTV2 (add1 low) high)))
;     ...))
;        
;;; Sample expressions for f-on-vector
;(define VECTOR0-VAL ...)
;...
;(define VECTORK-VAL ...)
;      
;;; Tests using sample computations for f-on-vector 
;(check-expect (f-on-vector VECTOR0 ...) VECTOR0-VAL)
;...
;(check-expect (f-on-vector VECTORK ...) VECTORK-VAL)
;     
;;; Tests using sample values for f-on-vector
;(check-expect (f-on-vector ... ...) ...)
;...


;; dot product

;; Sample (vectorof X)
(define VECTOR0 (vector))
(define VECTOR1 (vector 1 2 3))
(define VECTOR2 (vector -1 -2 8))
(define VECTOR3 (vector 5 10 20 40))
     
;; (vector X) (vector X) --> number.
;; Purpose: To compute the dot product of the given vectors
;; Assumption: Given vectors have the same length
(define (dot-product V1 V2)
  (local 
    [;; f-on-VINTV2: [int int] --> number
     ;; Purpose: Sum the product of vector elements in the given interval
     ;; ACCUM INV: sum = SIGMA V1[i]*V2[i] for i=0..low-1
     (define (sum-products low high sum)
       (if (> low high) 
           sum
           (sum-products (add1 low)
                         high
                         (+ (* (vector-ref V1 low)
                                            (vector-ref V2 low))
                            sum))))]
     (sum-products 0 (sub1 (vector-length V1)) 0)))
     
;; Tests using sample values for f-on-vector
(check-expect (dot-product VECTOR0 VECTOR0) 0)
(check-expect (dot-product VECTOR1 VECTOR2) 19)
(check-expect (dot-product VECTOR3 VECTOR3) 2125)