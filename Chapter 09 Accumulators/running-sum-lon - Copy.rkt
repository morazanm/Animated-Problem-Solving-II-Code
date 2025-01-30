;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname running-sum-lon) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define L0 '())
(define L1 (list 1 2 3 4 5 6))
(define L2 (build-list 2500 (λ (n) (random 100000))))

;; lon [int..int] --> lon
;; Purpose: Return the list element sum for the given interval
;; Assumption: The given interval only contains valid indices
;;             into the given lon
(define (lon-sum a-lon low high)
  (if (> low high)
      0
      (+ (list-ref a-lon high)
         (lon-sum a-lon low (sub1 high)))))

;; Sample expressions for lon-sum
(define SFN-L0-VAL 0)
(define SFN-L1-VAL (+ (list-ref L1 3)
                      (lon-sum L1 1 (sub1 3))))
(define SFN-L2-VAL (+ (list-ref L2 99)
                      (lon-sum L2 0 (sub1 99))))

;; Tests using sample computations for lon-sum
(check-expect (lon-sum L0 0 -1) SFN-L0-VAL)
(check-expect (lon-sum L1 1  3) SFN-L1-VAL)
(check-expect (lon-sum L2 0 99) SFN-L2-VAL)

;; Tests using sample values for lon-sum
(check-expect (lon-sum '(10 20 30 40) 0 1) 30)
(check-expect (lon-sum '(0 1 2 3 4 5) 0 5) 15)

;; lon [int..int] --> lon
;; Purpose: Return list of running totals for the
;;          given list and interval.
;; Assumption: The given interval contains only
;;             valid indices into the given list
(define (lon-running-totals-helper a-lon low high)
  (if (> low high)
      '()
      (cons (lon-sum a-lon 0 low)
            (lon-running-totals-helper a-lon (add1 low) high))))

;; Sample expressions for lon-running-totals-helper
(define LORSH-L0-5  '())
(define LORSH-L1-0  (cons (lon-sum L1 0 0)
                          (lon-running-totals-helper L1 (add1 0) 5)))
(define LORSH-L2-75 (cons (lon-sum L2 0 75)
                          (lon-running-totals-helper L2 (add1 75) 2499)))

;; Tests using sample computations for lon-running-totals-helper
(check-expect (lon-running-totals-helper L0 0 -1)    LORSH-L0-5)
(check-expect (lon-running-totals-helper L1 0 5)     LORSH-L1-0)
(check-expect (lon-running-totals-helper L2 75 2499) LORSH-L2-75)

;; Tests using sample values for lon-running-totals-helper
(check-expect (lon-running-totals-helper '(-2 -1 0 1 2) 0 4)
              '(-2 -3 -3 -2 0))
(check-expect (lon-running-totals-helper '(50 25 -40) 1 2)
              '(75 35))

;; lon --> lon
;; Purpose: Return the list of running totals for the given lon
(define (lon-running-totals a-lon)
  (lon-running-totals-helper a-lon 0 (sub1 (length a-lon))))

;; Sample expressions for lon-running-totals
(define LRS-L0 (lon-running-totals-helper L0 0 (sub1 (length L0))))
(define LRS-L1 (lon-running-totals-helper L1 0 (sub1 (length L1))))
(define LRS-L2 (lon-running-totals-helper L2 0 (sub1 (length L2))))

;; Tests using sample computations for lon-running-totals
(check-expect (lon-running-totals L0) LRS-L0)
(check-expect (lon-running-totals L1) LRS-L1)
(check-expect (lon-running-totals L2) LRS-L2)

;; Tests using sample values for lon-running-totals
(check-expect (lon-running-totals '(-1 0 1)) '(-1 -1 0))
(check-expect (lon-running-totals '(-5 2 4 0)) '(-5 -3 1 1))


;; lon number --> lon
;; Purpose: Return the list of running totals for the given lon
;;          using the given accumulator
;; Accumulator Invaraint: acc = previous running total
(define (lon-running-totals-helper-v2 a-lon acc)
  (if (empty? a-lon)
      '()
      (local [(define new-accum (+ (first a-lon) acc))]
        (cons new-accum
              (lon-running-totals-helper-v2
               (rest a-lon)
               new-accum)))))

;; Sample expressions for lon-running-totals-helper-v2
(define LRTHV2-L0-0 '())
(define LRTHV2-L1-0 (local [(define new-accum (+ (first L1) 0))]
                      (cons new-accum
                            (lon-running-totals-helper-v2
                             (rest L1)
                             new-accum))))
(define LRTHV2-L2-5 (local [(define new-accum (+ (first (rest L2)) (first L2)))]
                      (cons new-accum
                            (lon-running-totals-helper-v2
                             (rest (rest L2))
                             new-accum))))

;; Tests using sample computations for lon-running-totals-helper-v2
(check-expect (lon-running-totals-helper-v2 L0 0) LRTHV2-L0-0)
(check-expect (lon-running-totals-helper-v2 L1 0) LRTHV2-L1-0)
(check-expect (lon-running-totals-helper-v2 (rest L2) (first L2))
              LRTHV2-L2-5)

;; Tests using sample values for lon-running-totals-helper-v2
(check-expect (lon-running-totals-helper-v2 '(1 2 3)  0) '(1 3 6))
(check-expect (lon-running-totals-helper-v2
                (rest (rest '(10 4 5 6)))
                14)
              '(19 25))

;; lon --> lon
;; Purpose: Return the list of running totals for the given lon
(define (lon-running-totals-v2 a-lon)
  (lon-running-totals-helper-v2 a-lon 0))

;; Sample expressions for lon-running-totals2
(define LRS2-L0 (lon-running-totals-helper-v2 L0 0))
(define LRS2-L1 (lon-running-totals-helper-v2 L1 0))
(define LRS2-L2 (lon-running-totals-helper-v2 L2 0))

;; Tests using sample computations for lon-running-totals2
(check-expect (lon-running-totals-v2 L0) LRS2-L0)
(check-expect (lon-running-totals-v2 L1) LRS2-L1)
(check-expect (lon-running-totals-v2 L2) LRS2-L2)

;; Tests using sample values for lon-running-totals2
(check-expect (lon-running-totals-v2 '(-1 0 1)) '(-1 -1 0))
(check-expect (lon-running-totals-v2 '(-5 2 4 0)) '(-5 -3 1 1))

(define T1 (time (lon-running-totals  L2)))
(define T2 (time (lon-running-totals-v2 L2)))
