;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fibonacci) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;; natnum --> natnum
;; Purpose: Compute the nth Fibinacci number
(define (fib n)
  (if (< n 2)
      1
      (+ (fib (sub1 n)) (fib (- n 2)))))

;; Sample expressions for fib
(define FIB0 1)
(define FIB1 1)
(define FIB5 (+ (fib 4) (fib 3)))
(define FIB9 (+ (fib 8) (fib 7)))

;; Tests using sample computations for fib
(check-expect (fib 0) FIB0)
(check-expect (fib 1) FIB1)
(check-expect (fib 5) FIB5)
(check-expect (fib 9) FIB9)

;; Tests using sample values for fib
(check-expect (fib 4)  5)
(check-expect (fib 6) 13)


;; X --> X
;; Purpose: Return the given value
(define (endk v) v)


;; natnum (natnum --> natnum) --> natnum
;; Purpose: Compute the nth Fibinacci number
(define (fib/k n k)
  (if (< n 2)
      (k 1)
      (fib/k (sub1 n)
             (λ (f1)
               (fib/k (- n 2) (λ (f2) (k (+ f1 f2))))))))

;; Tests using sample values for fib/k
(check-expect (fib/k 0 endk) FIB0)
(check-expect (fib/k 1 endk) FIB1)
(check-expect (fib/k 5 endk) FIB5)
(check-expect (fib/k 9 endk) FIB9)
(check-expect (fib/k 4 endk)  5)
(check-expect (fib/k 6 endk) 13)





;; natnum --> natnum
;; Purpose: Compute the nth Fibonacci number
(define (fib-acc n)
  (local [;; [natnum natnum] natnum natnum --> natnum
          ;; Purpose: Compute the nth Fibinacci number
          ;; Accumulator Invariant
          ;;  f1 = the (low-1)th Fibonacci number AND f2 = the (low-2)th Fibobacci number
          (define (fib-helper low high f1 f2)
            (if (< high low)
                f1
                (fib-helper (add1 low) high (+ f1 f2) f1)))]
    (if (< n 2)
        1
        (fib-helper 2 n 1 1))))

;; Tests using sample values for fib-acc
(check-expect (fib-acc 0) FIB0)
(check-expect (fib-acc 1) FIB1)
(check-expect (fib-acc 5) FIB5)
(check-expect (fib-acc 9) FIB9)
(check-expect (fib-acc 4)  5)
(check-expect (fib-acc 6) 13)



(define N 35)
#|
(define T0 (time (fib N)))
(define T0K (time (fib/k N endk)))
(define T1 (time (fib N)))
(define T1K (time (fib/k N endk)))
(define T2 (time (fib N)))
(define T2K (time (fib/k N endk)))
(define T3 (time (fib N)))
(define T3K (time (fib/k N endk)))
(define T4 (time (fib N)))
(define T4K (time (fib/k N endk)))
|#

(define T0A (time (fib-acc N)))
(define T1A (time (fib-acc N)))
(define T2A (time (fib-acc N)))
(define T3A (time (fib-acc N)))
(define T4A (time (fib-acc N)))