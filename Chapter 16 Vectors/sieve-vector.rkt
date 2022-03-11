;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sieve-vector) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require while)

;; natnum --> (listof natmum)
;; Purpose: Return all the primes <= to the given natnum
(define (sieve-vector n)
  (local [;; (vectorof Boolean)
          ;; Purpose: Track primes identified
          (define is-prime (build-vector (add1 n) (λ (i) #true)))

          ;; natnum
          ;; Purpose: Index of next potential prime
          (define low (void))

          (define high (quotient n 2))

          ;; natnum [low high] --> (void)
          ;; Purpose: Mark multiples of the given low as not primes
          ;; How: Mutate is-prime[low] to be false and create a new
          ;;      vector interval to traverse starting with the next
          ;;      multiple of i by adding i to low
          ;; Effect: In is-primes multiples of given i in [low..high] are set to #false
          ;; Assumption: low is a multiple of i
          (define (mark-multiples! i low high)
            (if (> low high)
                (void)
                (begin
                  (vector-set! is-prime low #false)
                  (mark-multiples! i (+ i low) high))))
          ;; Termination Argument
          ;; The given vector interval is [low..high]. Each recursive call
          ;; is made with a smaller interval: [i+low..high]. Eventually,
          ;; the given vector interval becomes empty and the function halts.

          ;; [low high] --> (listof natnum)
          ;; Purpose: Return the list of primes <= n
          (define (extract-primes low high)
            (cond [(> low high) '()]
                  [(vector-ref is-prime low)
                   (cons low (extract-primes (add1 low) high))]
                  [else (extract-primes (add1 low) high)]))
                

          ]
    (if (< n 2)
        '()
        (begin
          (set! low 2)
          ;; INV
          ;; For j in [2..n],     (not is-prime[j]) ==> there is a proper divisor of j in [2..low-1]
          ;;                  AND is-prime[j] ==> no proper divisor of j in [2..low-1]
          ;; AND 2 <= low <= high + 1                 
          (while (<= low high)
                 ;; For j in [2..n], (not is-prime[j]) ==> there is a proper divisor of j in [2..low-1]
                 ;;                  is-prime[j] ==> no proper divisor of j in [2..low-1]
                 ;; AND 2 <= i < high + 1
                 (if (vector-ref is-prime low)
                     (begin
                       ;; For j in [2..n],     (not is-prime[j]) ==> there is a proper divisor of j in [2..low-1]
                       ;;                  AND is-prime[j] ==> no proper divisor of j in [2..low-1]
                       ;; AND 2 <= low < high + 1
                       ;; AND low is a prime
                       (mark-multiples! low (* 2 low) n)
                       ;; For j in [2..n],     (not is-prime[j]) ==> there is a proper divisor of j in [2..low]
                       ;;                  AND is-prime[j] ==> no proper divisor of j in [2..low]
                       ;; AND 2 <= low < high + 1
                       (set! low (add1 low))
                       ;; INV
                       )
                     ;; else branch
                     ;; For j in [2..n], (not is-prime[j]) ==> there is a proper divisor of j in [2..low-1]
                     ;;                  AND is-prime[j] ==> no proper divisor of j in [2..low-1]
                     ;; AND 2 <= low < high + 1
                     ;; AND low is not prime
                     (set! low (add1 low))
                     ;; INV
                     ))
          ;; For j in [2..n],     (not is-prime[j]) ==> there is a proper divisor of j in [2..low-1]
          ;;                  AND is-prime[j] ==> no proper divisor of j in [2..low-1]
          ;; AND 2 <= low <= high + 1
          ;; AND low > high
          ;; ==> low = high + 1
          ;; ==> For j in [2..n],     (not is-prime[j]) ==> there is a proper divisor of j in [2..high]
          ;;                      AND is-prime[j] ==> no proper divisor of j in [2..high]
          ;; For j in [2..n], (not is-prime[j]) ==> j is not a prime
          ;;                  AND is-prime[j] ==> j is prime
          (extract-primes 2 n)
          ;; Termination Argument
          ;; The loop traverses [2..(quotient n 2)] from the low end to
          ;; the high end. The traversal starts with low equal to 2. 
          ;; Every time the body of the loop is evaluated, regardless
          ;; of which path is taken in the if-expr, low is
          ;; incremented reducing the size of the vector interval to
          ;; traverse by 1. Eventually, the vector interval becomes empty
          ;; when low = (quotient n 2) + 1 and the loop halts.
          ))))

;; Tests for sieve-vector
(check-expect (sieve-vector 0)  '())
(check-expect (sieve-vector 1)  '())
(check-expect (sieve-vector 11) '(2 3 5 7 11))
(check-expect (sieve-vector 20) '(2 3 5 7 11 13 17 19))

(define T2 (time (sieve-vector 50000)))


