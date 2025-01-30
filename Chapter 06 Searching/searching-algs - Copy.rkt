;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname searching-algs) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))


(define L0 '())
(define L1 '(88 54 4 7 87 98 -7 0 -1))
(define L2 '(9 8 7 6 5  4  3  2 1  0 -1 -2))
(define L3 (build-list 1000000 (λ (i) (random 1000000))))
(define L4 (build-list 1000000 (λ (i) i)))

(define L1S (sort L1 <))
(define L2S (sort L2 <))
(define L3S (sort L3 <))

;; A result, res, is either:
;;  1. natnum
;;  2. #false

;; number lon --> res
;; Purpose: Return the index of the first occurrence of the
;;          given number if it is a member of the given list.
;;          Otherswise, return #false
(define (linear-search a-num a-lon)
  (cond [(empty? a-lon) #false]
        [(= a-num (first a-lon)) 0]
        [else
         (local [(define result-of-rest (linear-search a-num (rest a-lon)))]
           (if (false? result-of-rest)
               #false
               (add1 result-of-rest)))]))

;; Sample expressions for linear-search
(define LS-L0-VAL  #false)
(define LS-L1-VAL1 0)
(define LS-L2-VAL1 0)
(define LS-L1-VAL2 (local [(define result-of-rest (linear-search -9 (rest L1)))]
                     (if (false? result-of-rest)
                         #false
                         (add1 result-of-rest))))
(define LS-L2-VAL2 (local [(define result-of-rest (linear-search 54 (rest L2)))]
                     (if (false? result-of-rest)
                         #false
                         (add1 result-of-rest))))
(define LS-L1-VAL3 (local [(define result-of-rest (linear-search -7 (rest L1)))]
                     (if (false? result-of-rest)
                         #false
                         (add1 result-of-rest))))
(define LS-L2-VAL3 (local [(define result-of-rest (linear-search  2 (rest L2)))]
                     (if (false? result-of-rest)
                         #false
                         (add1 result-of-rest))))


;; Tests using sample computations for linear-search
(check-expect (linear-search 25 L0) LS-L0-VAL)
(check-expect (linear-search 88 L1) LS-L1-VAL1)
(check-expect (linear-search  9 L2) LS-L2-VAL1)
(check-expect (linear-search -9 L1) LS-L1-VAL2)
(check-expect (linear-search 54 L2) LS-L2-VAL2)
(check-expect (linear-search -7 L1) LS-L1-VAL3)
(check-expect (linear-search  2 L2) LS-L2-VAL3)

;; Tests using sample values for linear-search
(check-satisfied
 (linear-search 100 L3)
 (λ (a-res) (or (false? a-res)
                (= (list-ref L3 a-res) 100))))
(check-expect (linear-search 2 '(1 2 3)) 1)
(check-expect (linear-search 5 '(1 2 3)) #false)
(check-expect (linear-search 2000000 L4) #false)
(check-expect (linear-search 998999  L4) 998999)


;; number [int>=0..int>=-1] lon --> res
;; Purpose: Return the index of the given number if it 
;;          is a member of the given list. Otherwise,
;;          return #false
;; How: If the given interval is empty the given number
;;  is not in the given list and return #false. Otherwise,
;;  compute the middle index and return it if the given
;;  list has the given number at that index. If not
;;  search either the left subtree or the right subtree.
;; Assumption: The given lon is sorted in nondecreasing order
;;             and given interval only contains valid indices
;;             into the given lon
(define (bin-search a-num low high a-lon)
  (if (< high low)
      #false
      (local [(define mid-index (quotient (+ low high) 2))]
        (cond [(= (list-ref a-lon mid-index) a-num) mid-index]
              [(> (list-ref a-lon mid-index) a-num)
               (bin-search a-num low (sub1 mid-index) a-lon)]
              [else (bin-search a-num (add1 mid-index) high a-lon)]))))

;; Sample expressions for bin-search
(define BINS-L0-VAL1  #false)
(define BINS-L3S-VAL1 #false)
(define BINS-L1S-VAL1 (local [(define mid-index (quotient (+ 0  8) 2))]
                        mid-index))
(define BINS-L2S-VAL1 (local [(define mid-index (quotient (+ 3 7) 2))]
                        mid-index))
(define BINS-L1S-VAL2 (local [(define mid-index (quotient (+ 0 8) 2))]
                        (bin-search  0 0 (sub1 mid-index) L1S)))
(define BINS-L2S-VAL2 (local [(define mid-index (quotient (+ 0 11) 2))]
                        (bin-search -6 0 (sub1 mid-index) L2S)))
(define BINS-L1S-VAL3 (local [(define mid-index (quotient (+ 0 8) 2))]
                        (bin-search  90 (add1 mid-index)  8 L1S)))
(define BINS-L2S-VAL3 (local [(define mid-index (quotient (+ 0 11) 2))]
                        (bin-search  8 (add1 mid-index) 11 L2S)))

;; Tests using sample computations for bin-search
(check-expect (bin-search 65 0 -1 L0)  BINS-L0-VAL1)
(check-expect (bin-search -9 5  4 L3S) BINS-L3S-VAL1)
(check-expect (bin-search  7 0  8 L1S) BINS-L1S-VAL1)
(check-expect (bin-search  3 3  7 L2S) BINS-L2S-VAL1)
(check-expect (bin-search  0 0  8 L1S) BINS-L1S-VAL2)
(check-expect (bin-search -6 0 11 L2S) BINS-L2S-VAL2)
(check-expect (bin-search 90 0  8 L1S) BINS-L1S-VAL3)
(check-expect (bin-search  8 0 11 L2S) BINS-L2S-VAL3)

;; Tests using sample values for bin-search
(check-satisfied
 (bin-search 100 0 (sub1 10000) L3S)
 (λ (a-res) (or (false? a-res)
                (= (list-ref L3S a-res) 100))))
(check-expect (bin-search 2 0 2 '(1 2 3)) 1)
(check-expect (bin-search 5 0 5 '(1 2 3 4 6 7)) #false)

;; number lon --> res
;; Purpose: Return the index of the given number if it 
;;          is a member of the given list. Otherswise,
;;          return #false
;; Assumption: The given lon is sorted in nondecreasing order
(define (binary-search a-num a-lon)
  (bin-search a-num 0 (sub1 (length a-lon)) a-lon))

;; Sample expressions for binary-search
(define BS-L0-VAL  (bin-search 25 0 (sub1 (length L0)) L0))
(define BS-L1-VAL1 (bin-search 88 0 (sub1 (length L1S)) L1S))
(define BS-L2-VAL1 (bin-search  9 0 (sub1 (length L2S)) L2S))
(define BS-L1-VAL2 (bin-search -9 0 (sub1 (length L1S)) L1S))
(define BS-L2-VAL2 (bin-search 54 0 (sub1 (length L2S)) L2S))
(define BS-L1-VAL3 (bin-search -7 0 (sub1 (length L1S)) L1S))
(define BS-L2-VAL3 (bin-search  2 0 (sub1 (length L2S)) L2S))


;; Tests using sample computations for binary-search
(check-expect (binary-search 25 L0) BS-L0-VAL)
(check-expect (binary-search 88 L1S) BS-L1-VAL1)
(check-expect (binary-search  9 L2S) BS-L2-VAL1)
(check-expect (binary-search -9 L1S) BS-L1-VAL2)
(check-expect (binary-search 54 L2S) BS-L2-VAL2)
(check-expect (binary-search -7 L1S) BS-L1-VAL3)
(check-expect (binary-search  2 L2S) BS-L2-VAL3)

;; Tests using sample values for binary-search
(check-satisfied
 (binary-search 100 L3S)
 (λ (a-res) (or (false? a-res)
                (= (list-ref L3S a-res) 100))))
(check-expect (binary-search 2 '(1 2 3)) 1)
(check-expect (binary-search 5 '(1 2 3)) #false)
(check-expect (binary-search 2000000 L4) #false)
(check-expect (binary-search 998999  L4) 998999)



(display "L0")
(newline)
(define LSL0 (time (linear-search 83333 L0)))
(define BSL0 (time (binary-search 83333 L0)))
(newline)
(newline)
(display "L1")
(newline)
(define LSL1 (time (linear-search 0 L1)))
(define BSL1 (time (binary-search 0 L1S)))
(newline)
(newline)
(display "L2")
(newline)
(define LSL2 (time (linear-search 8 L2)))
(define BSL2 (time (binary-search 8 L2S)))
(newline)
(display "L3")
(newline)
;(define VAL (random 1000000))
(define LSL3 (time (linear-search (first L3) L3)))
(define BSL3 (time (binary-search (first L3) L3S)))
(newline)
(newline)
(display "L4")
(newline)
(define LSL4 (time (linear-search 2000000 L4)))
(define BSL4 (time (binary-search 2000000 L4)))
(newline)





