;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sieve-of-Eratosthenes) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;; Sample natnums
(define ZERO  0)
(define ONE   1)
(define FIVE  5)
(define SIX   6)
(define SEVEN 7)
(define 12K   12000)

(define L1 '(17 19 23 29 31 37 41 43 47 49 53 59 61 67))
(define L2 '(3 5))
(define L3 '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(define L4 '(3 5 7 9 11 13 15 17 19 21 23))

(define (all-primes<=n n)
  (local [;; natnum --> Boolean
          ;; Purpose: Determine if given natnum is a prime
          (define (prime? n)
            (local [;; [int int] --> Boolean
                    ;; Purpose: Determine if n is divisible by any number
                    ;;          in the given interval
                    (define (any-divide? low high)
                      (if (< high low)
                          #false
                          (or (= (remainder n high) 0)
                              (any-divide? low (sub1 high)))))]
              (if (< n 2)
                  #false
                  (not (any-divide? 2 (quotient n 2))))))]
    (cond [(= n 0) '()]
          [(prime? n) (cons n (all-primes<=n (sub1 n)))]
          [else (all-primes<=n (sub1 n))])))

;; Sample expressions for all-primes<=n
(define ZERO-VAL  '())
(define ONE-VAL   '())
(define FIVE-VAL  (cons FIVE (all-primes<=n (sub1 FIVE))))
(define SEVEN-VAL (cons SEVEN (all-primes<=n (sub1 SEVEN))))
(define SIX-VAL   (all-primes<=n (sub1 SIX)))
(define 12K-VAL   (all-primes<=n (sub1 12K)))

;; Tests using sample computations for all-primes<=n
(check-expect (all-primes<=n ZERO)  ZERO-VAL)
(check-expect (all-primes<=n ONE)   ONE-VAL)
(check-expect (all-primes<=n FIVE)  FIVE-VAL)
(check-expect (all-primes<=n SEVEN) SEVEN-VAL)
(check-expect (all-primes<=n 12K)   12K-VAL)

;; Tests using sample values for all-primes<=n
(check-expect (all-primes<=n 17) '(17 13 11 7 5 3 2))
(check-expect (all-primes<=n 3)  '(3 2))


;; SIEVE OF ERATOSTHENES
;; natnum --> (listof natnum)
;; Purpose: Compute all primes <= to given natnum
(define (the-primes<=n n)
  (if (< n 2)
      '()
      (sieve (rest (rest (build-list (add1 n) (λ (i) i)))))))

;; (listof natnum) \arrow (listof natnum)
;; Purpose: Extract the prime numbers in the given list
;; Assumption: The given list is nonempty, its first
;;   element is prime, and contains no numbers that
;;   are divisible by a number less than the first
;;   element.
;; How: If the rest of the list is shorter than the given
;;   list's first number stop. Otherwise, add the first
;;   number to the result and repeat the process
;;   by removing the multiples of the first element from
;;   the rest of the given list.
(define (sieve lon)
  (if (< (length (rest lon)) (first lon))
      lon
      (local
        [(define new-inst (filter
                           (λ (n)
                             (not (= (remainder n (first lon)) 0)))
                           (rest lon)))]
        (cons (first lon) (sieve new-inst)))))


       
;; Sample expressions for the-primes<=n
(define ZERO-VALUE  '())
(define ONE-VALUE   '())
(define FIVE-VALUE (sieve (rest (rest (build-list (add1 FIVE) (λ (i) i))))))
(define SEVEN-VALUE (sieve (rest (rest (build-list (add1 SEVEN)(λ (i) i))))))
(define SIX-VALUE (sieve (rest (rest (build-list (add1 SIX)(λ (i) i))))))
(define 12K-VALUE (sieve (rest (rest (build-list (add1 12K) (λ (i) i))))))

;; Tests using sample computations for the-primes<=n
(check-expect (the-primes<=n ZERO)  ZERO-VALUE)
(check-expect (the-primes<=n ONE)   ONE-VALUE)
(check-expect (the-primes<=n FIVE)  FIVE-VALUE)
(check-expect (the-primes<=n SEVEN) SEVEN-VALUE)
(check-expect (the-primes<=n 12K)   12K-VALUE)

;; Tests using sample values for the-primes<=n2
(check-expect (the-primes<=n 17) '(2 3 5 7 11 13 17))
(check-expect (the-primes<=n 3)  '(2 3))


;; Sample expressions for sieve
(define L1-VAL L1)
(define L2-VAL L2)
(define L3-VAL
  (local
    [(define new-inst
       (filter
        (λ (n)
          (not (= (remainder n (first L3)) 0)))
        (rest L3)))]
    (cons (first L3) (sieve new-inst))))
(define L4-VAL
  (local
    [(define new-inst
       (filter
        (λ (n)
          (not (= (remainder n (first L4)) 0)))
        (rest L4)))]
    (cons (first L4) (sieve new-inst))))

;; Tests using sample computations for sieve
(check-expect (sieve L1) L1-VAL)
(check-expect (sieve L2) L2-VAL)
(check-expect (sieve L3) L3-VAL)
(check-expect (sieve L4) L4-VAL)

;; Tests using sample computations for sieve
(check-expect (sieve '(7 11 13 17 19 23 29 31 37 41 43 47
                         49 53 59 61 67))
              '(7 11 13 17 19 23 29 31 37 41 43 47 53 59
                  61 67))
(check-expect (sieve '(5 7 11 13 17 19 23 25 29 31 35))
              '(5 7 11 13 17 19 23 29 31))

(define T3 (time (all-primes<=n 50000)))
(define T4 (time (the-primes<=n 50000)))