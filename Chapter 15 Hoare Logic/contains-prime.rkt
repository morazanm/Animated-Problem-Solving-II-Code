;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname contains-prime) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require while)

;; [integer integer] --> Boolean
;; Purpose: Determine if the given interval contains a prime
;; Assumption: The given interval is not empty
(define (contains-prime? low high)
  (local [;; integer
          ;; Purpose: Next integer to test
          (define l (void))

          ;; Boolean
          ;; Purpose: Indicate if a prime is found
          (define found (void))

          

          ;; integer --> Boolean
          ;; Purpose: Determine if given integer is a prime
          (define (prime? n)
            (local [;; [int int] --> Boolean
                    ;; Purpose: Determine if n is divisible by any number
                    ;; in the given interval
                    (define (any-divide? low high)
                      (if (< high low)
                          #false
                          (or (= (remainder n high) 0)
                              (any-divide? low (sub1 high)))))]
              (if (< n 2)
                  #false
                  (not (any-divide? 2 (quotient n 2))))))]
    (begin
      (set! l low)
      (set! found #false)
      ;; INV: l <= high+1 AND found ==> prime in [low..l-1] AND (not found) ==> no prime in [low..l-1]
      (while (>= high l)
             ;; l <= high AND found ==> prime in [low..l-1] AND (not found) ==> no prime in [low..l-1]
             (set! found (or (prime? l) found))
             ;; l <= high AND found ==> prime in [low..l] AND (not found) ==> no prime in [low..l]
             (set! l (add1 l))
             ;; l <= high+1 AND found ==> prime in [low..l-1] AND (not found) ==> no prime in [low..l-1]
             )
      ;; l <= high+1 AND found ==> prime in [low..l-1] AND (not found) ==> no prime in [low..l-1] AND (l > high)
      ;; ==> l = high+1
      ;; ==> found ==> prime in [low..high] AND (not found) ==> no prime in [low..high]
      found)
    ;; Termination Argument
    ;; l starts at low. Each loop iteration increases l by 1.
    ;; Eventually, [l..high] becomes empty when l = high + 1
    ;; and the loop terminates
    ))

;; Tests for contains-prime?
(check-expect (contains-prime? 3 2)   #false)
(check-expect (contains-prime? 44 46) #false)
(check-expect (contains-prime? -9 -4) #false)
(check-expect (contains-prime? -5 5)  #true)
(check-expect (contains-prime? 19 19) #true)

             
    