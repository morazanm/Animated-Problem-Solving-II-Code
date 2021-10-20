;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname factorial) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;; natnum --> natnum
;; Purpose: Compute the factorial of the given natural number
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (sub1 n)))))

;; Sample expressions for fact
(define F0 1)
(define F5 (* 5 (fact (sub1 5))))
(define F10 (* 10 (fact (sub1 10))))

;; Tests using sample computations for fact
(check-expect (fact 0)  F0)
(check-expect (fact 5)  F5)
(check-expect (fact 10) F10)

;; Tests using sample values for fact
(check-expect (fact 2)  2)
(check-expect (fact 6)  720)



#|
(check-expect (fact-accum 0  1) 1)
(check-expect (fact-accum 5  1) 120)
(check-expect (fact-accum 10 1) 3628800)
|#

;; natnum --> natnum
;; Purpose: Compute the factorial of the given natural number
(define (fact2 n)
  (local [;; natnum natnum --> natnum
          ;; Purpose: Compute the factorial of the first given natural number
          ;; Accumulator invariant
          ;;    accum = the product of the natural numbers in [k+1..n]
          (define (fact-accum k accum)
            (if (= k 0)
                accum
                (fact-accum (sub1 k) (* k accum))))]
    (fact-accum n 1)))

;; Tests using sample values for fact2
(check-expect (fact2 0)  1)
(check-expect (fact2 5)  120)
(check-expect (fact2 10) 3628800)
(check-expect (fact 2)  2)
(check-expect (fact 6)  720)

(define F  (time (fact 20000)))
(define F2 (time (fact2 20000)))

