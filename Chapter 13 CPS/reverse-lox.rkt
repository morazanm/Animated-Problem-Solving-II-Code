;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname reverse-lox) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define ELOX '())
(define LOX1 '(d c b a))
(define LOX2 '(99 #true "Hi!"))

;; (listof X) --> (listof X)
;; Purpose: Reverse the given list
(define (rev-lox a-lox)
  (if (empty? a-lox)
      '()
      (append (rev-lox (rest a-lox))
              (list (first a-lox)))))

;; Sample expressions for rev-lox
(define REV-ELOX '())
(define REV-LOX1 (append (rev-lox (rest LOX1))
                         (list (first LOX1))))
(define REV-LOX2 (append (rev-lox (rest LOX2))
                         (list (first LOX2))))

;; Tests using sample computations for rev-lox
(check-expect (rev-lox ELOX) REV-ELOX)
(check-expect (rev-lox LOX1) REV-LOX1)
(check-expect (rev-lox LOX2) REV-LOX2)



;; (listof X) → (listof X)
;; Purpose: Reverse the first given list
;; Accumulator invariant:
;; accum = the list of traversed elements reversed
(define (rev-lox-accum a-lox accum)
  (if (empty? a-lox)
      accum
      (rev-lox-accum (rest a-lox) (cons (first a-lox) accum))))

;; Tests using sample computations for rev-lox-accum
(check-expect (rev-lox-accum ELOX '())
              REVACCUM-ELOX1)
(check-expect (rev-lox-accum ELOX '(blue white red))
              REVACCUM-ELOX2)
(check-expect (rev-lox-accum LOX1 '())
              REVACCUM-LOX1)
(check-expect (rev-lox-accum (rest LOX2)
                             (list (first LOX2)))
              REVACCUM-RLOX2)

;; Tests using sample values for rev-lox-accum
(check-expect (rev-lox-accum '(1 2 3 4) '())
              '(4 3 2 1))
(check-expect (rev-lox-accum '(#false #true) '())
              '(#true #false))

;; Sample expressions for rev-lox-accum
(define REVACCUM-ELOX1 '())
(define REVACCUM-ELOX2 '(blue white red))

(define REVACCUM-LOX1 (rev-lox-accum
                       (rest LOX1)
                       (cons (first LOX1) '())))
(define REVACCUM-RLOX2 (rev-lox-accum
                        (rest (rest LOX2))
                        (cons (first (rest LOX2))
                              (list (first LOX2)))))


;; (listof X) → (listof X)
;; Purpose: Reverse the given list
(define (rev-lox2 a-lox)
  (rev-lox-accum a-lox '()))

;; Sample expressions for rev-lox2
(define REV2-ELOX (rev-lox-accum ELOX '()))
(define REV2-LOX1 (rev-lox-accum LOX1 '()))
(define REV2-LOX2 (rev-lox-accum LOX2 '()))

;; Tests using sample computations for rev-lox2
(check-expect (rev-lox2 ELOX) REV2-ELOX)
(check-expect (rev-lox2 LOX1) REV2-LOX1)
(check-expect (rev-lox2 LOX2) REV2-LOX2)

;; Tests using sample values for rev-lox2
(check-expect (rev-lox2 '(1 2 3 4 5))
              '(5 4 3 2 1))
(check-expect (rev-lox2 '(red white blue))
              '(blue white red))





;(define (rev/k L k)
;  (if (empty? L)
;      (k '())
;      (k (append (rev (rest L)) (list (first L))))))

;; X --> X
;; Purpose: Return the given value
(define (endk v) v)

;; (listof X) ((listof X) \arrow (listof X)) \arrow (listof X)
;; Purpose: Reverse the given list
(define (rev-lox/k a-lox k)
  (if (empty? a-lox)
      (k '())
      (rev-lox/k (rest a-lox) (λ (revr) (k (append revr (list (first a-lox))))))))

;; Sample expressions for rev-lox/k
(define REV-ELOXK '())
(define REV-LOXK1 (rev-lox/k
                   (rest LOX1)
                   (λ (revr)
                     (endk (append revr (list (first LOX1)))))))
(define REV-LOXK2 (rev-lox/k
                   (rest LOX2)
                   (λ (revr)
                     (endk (append revr (list (first LOX2)))))))

;; Tests using sample computations for rev-lox/k
(check-expect (rev-lox/k ELOX endk) REV-ELOXK)
(check-expect (rev-lox/k LOX1 endk) REV-LOXK1)
(check-expect (rev-lox/k LOX2 endk) REV-LOXK2)

;; Tests using sample values for rev-lox/k
(check-expect (rev-lox/k '(1 2 3 4 5) endk)
              '(5 4 3 2 1))
(check-expect (rev-lox/k '(red white blue) endk)
              '(blue white red))


;; Tests
(check-expect (rev-lox/k '() endk) '())
(check-expect (rev-lox/k '(1 2 3 4 5) endk) '(5 4 3 2 1))
(check-random (rev-lox/k (build-list 1500 (λ (i) (random 100000))) endk)
              (reverse (build-list 1500 (λ (i) (random 100000)))))

(define L (build-list 50000 (λ (i) (random 100000))))
;;(define TESTLIST (build-list 10000 (λ (i) (random 100000))))

(define T1 (time (rev-lox L)))
(define T1A (time (rev-lox2 L)))
(define T1K (time (rev-lox/k L endk)))
(newline)
(define T2 (time (rev-lox L)))
(define T2A (time (rev-lox2 L)))
(define T2K (time (rev-lox/k L endk)))
(newline)
(define T3 (time (rev-lox L)))
(define T3A (time (rev-lox2 L)))
(define T3K (time (rev-lox/k L endk)))
(newline)
(define T4 (time (rev-lox L)))
(define T4A (time (rev-lox2 L)))
(define T4K (time (rev-lox/k L endk)))
(newline)
(define T5 (time (rev-lox L)))
(define T5A (time (rev-lox2 L)))
(define T5K (time (rev-lox/k L endk)))
(newline)













