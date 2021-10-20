;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname reverse-lox) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

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

;; Tests using sample values for rev-lox
(check-expect (rev-lox '(1 2 3 4 5))
              '(5 4 3 2 1))
(check-expect (rev-lox '(red white blue))
              '(blue white red))

;; (listof X) --> (listof X)
;; Purpose: Reverse the first given list
;; Accumulator invariant:
;;  accum = the list of traversed elements reversed
(define (rev-lox-accum a-lox accum)
  (if (empty? a-lox)
      accum
      (rev-lox-accum
       (rest a-lox)
       (cons (first a-lox) accum))))

;; Sample expressions for rev-lox-accum
(define SUMACCUM-ELOX1 '())
(define SUMACCUM-ELOX2 '(blue white red))
(define SUMACCUM-LOX1 (rev-lox-accum
                        (rest LOX1)
                        (cons (first LOX1) '())))
(define SUMACCUM-RLOX2 (rev-lox-accum
                        (rest (rest LOX2))
                        (cons (first (rest LOX2))
                              (list (first LOX2)))))

;; Tests using sample computations for rev-lox-accum
(check-expect (rev-lox-accum ELOX '())
              SUMACCUM-ELOX1)
(check-expect (rev-lox-accum ELOX '(blue white red))
              SUMACCUM-ELOX2)
(check-expect (rev-lox-accum LOX1 '()) SUMACCUM-LOX1)
(check-expect (rev-lox-accum (rest LOX2)
                             (list (first LOX2)))
              SUMACCUM-RLOX2)

;; Tests using sample values for rev-lox-accum
(check-expect (rev-lox-accum '(1 2 3 4) '())
              '(4 3 2 1))
(check-expect (rev-lox-accum '(#false #true)  '())
              '(#true #false))

;; (listof X) --> (listof X)
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

;; (listof X) --> (listof X)
;; Purpose: Reverse the given list
(define (rev-lox3 a-lox)
  (foldl cons '() a-lox))

;; Sample expressions for rev-lox3
(define REV3-ELOX (rev-lox3 ELOX))
(define REV3-LOX1 (rev-lox3 LOX1))
(define REV3-LOX2 (rev-lox3 LOX2))

;; Tests using sample computations for rev-lox3
(check-expect (rev-lox3 ELOX) REV3-ELOX)
(check-expect (rev-lox3 LOX1) REV3-LOX1)
(check-expect (rev-lox3 LOX2) REV3-LOX2)

;; Tests using sample values for rev-lox2
(check-expect (rev-lox3 '(1 2 3 4 5))
              '(5 4 3 2 1))
(check-expect (rev-lox3 '(red white blue))
              '(blue white red))


(define L (build-list 50000 (λ (i) (random 100000))))

(define T1 (time (rev-lox  L)))
(define T2 (time (rev-lox2 L)))
(define T3 (time (rev-lox3 L)))

