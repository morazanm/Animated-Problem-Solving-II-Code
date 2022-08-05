;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname reverse-vector) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define L0 (list))
(define L1 (list 1 2 3 4 5))
(define L2 (build-list 5000 (λ (i) (make-posn i i))))

;; (listof X) → (listof X)
;; Purpose: Reverse the given list
(define (rev-lox a-lox)
  (if (empty? a-lox)
      '()
      (append (rev-lox (rest a-lox)) (list (first a-lox)))))

;; Sample expressions for rev-lox
(define L0-VAL '())
(define L1-VAL (append (rev-lox (rest L1)) (list (first L1))))
(define L2-VAL (append (rev-lox (rest L2)) (list (first L2))))

;; Tests using sample computations for rev-lox
(check-expect (rev-lox L0) L0-VAL)
(check-expect (rev-lox L1) L1-VAL)
(check-expect (rev-lox L2) L2-VAL)

;; Tests using sample values for rev-lox
(check-expect (rev-lox '(-3 8 -100 88)) '(88 -100 8 -3))
(check-expect (rev-lox (list (make-posn 1 2) (make-posn 2 2)))
              (list (make-posn 2 2) (make-posn 1 2)))


;; (listof X) --> (listof X)
;; Purpose: Reverse the given list
(define (rev-lox2 L)
  (local [;; (listof X) (listof X) --> (listof X)
          ;; Purpose: Reverse L
          ;; Acc INV: res = reverse L - l
          (define (rev-helper l res)
            (if (empty? l)
                res
                (rev-helper (rest l) (cons (first l) res))))]
    (rev-helper L '())))

;; Tests using for rev-lox2
(check-expect (rev-lox2 L0) '())
(check-expect (rev-lox2 L1) '(5 4 3 2 1))
(check-expect (rev-lox2 L2) (build-list 5000 (λ (i) (make-posn (- 5000 i 1) (- 5000 i 1)))))




(define V0 (vector))
(define V1 (vector 1 2 3 4 5))
(define V2 (build-vector 5000 (λ (i) (make-posn i i))))

;(define (helper V low high)
;  (if (> low high)
;      (void)
;      (begin
;        (swap! V low (sub1 (- (vector-length V) low)))
;        (helper V (add1 low) high))))
;(define VV0 (vector))
;(define VV1 (vector 1 2 3 4))
;(define VV2 (vector 1 2 3 4 5 6 7 8 9))
;(check-expect (begin
;                (helper VV0 0 -1)
;                VV0)
;              (vector))
;(check-expect (begin
;                (helper VV1 0 1)
;                VV1)
;              (vector 4 3 2 1))
;(check-expect (begin
;                (helper VV2 0 3)
;                VV2)
;              (vector 9 8 7 6 5 4 3 2 1))

;; (vectorof X) --> (void)
;; Purpose: Reverse the elements in the given vector
;; Effect: Vector elements are rearranged such that
;;         For all i in [0..(quotient n 2)] V[i] is
;;         swapped with V[(vector-length V)-i-1]
(define (reverse-vector-in-place! V)
  (local [;; natnum natnum --> (void)
          ;; Purpose: Swap the elements at the given indices
          ;; Effect: V is mutated by swapping elements at given indices
          (define (swap! i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))

          ;; [int int] --> (void)
          ;; Purpose: Reverse the elements of V
          ;; Effect: Vector elements in the given vector interval
          ;;         are rearranged such that  V[i] is swapped
          ;;         with V[(vector-length V)-low-1]
          ;; Assumption: low >= 0 AND 2*high is a valid index into V
          (define (rev-vector! low high)
            (if (> low high)
                (void)
                (begin
                  (swap! low (sub1 (- (vector-length V) low)))
                  (rev-vector! (add1 low) high))))]
    (rev-vector! 0 (sub1 (quotient (vector-length V) 2)))))

;; Tests for reverse-vector-in-place!
(check-expect (begin
                (reverse-vector-in-place! V0)
                V0)
              (vector))
(check-expect (begin
                (reverse-vector-in-place! V1)
                V1)
              (vector 5 4 3 2 1))
(check-expect (begin
                (reverse-vector-in-place! V2)
                V2)
              (build-vector 5000 (λ (i) (make-posn (- 5000 i 1) (- 5000 i 1)))))

;(define L4rev-lox  (build-list   50000 (λ (i) i)))
(define L4rev-lox2 (build-list   5000000 (λ (i) i)))
(define V4rv-in-p  (build-vector 5000000 (λ (i) i)))

;(define T1 (time (rev-lox L4rev-lox)))
(define T2 (time (rev-lox2 L4rev-lox2)))
(define T3 (time (reverse-vector-in-place! V4rv-in-p)))