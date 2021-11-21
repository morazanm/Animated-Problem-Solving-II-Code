;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname quick-sort-k) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define LON0 '())
(define LON1 '(71 81 21 28 72 19 49 64 4 47 81 4))
(define LON2 '(91 57 93 5 16 56 61 59 93 49 -3))
(define LON3 (build-list 2500 (λ (i) (- 100000 i))))
(define LON4 (build-list 200  (λ (i) (random 100000000))))
(define LON5 (build-list 1575 (λ (i) (random 10000000))))
(define LON6 (build-list 1575 (λ (i) i)))

;; lon --> lon
;; Purpose: Sort given lon in nondecreasing order
;; How: When the given list is empty stop and return the
;;   empty list. Otherwise, place the given's list
;;   first number between the sorted numbers less than
;;   or equal to the first number and the sorted numbers
;;   greater than the first number.
(define (quick-sorting a-lon)
  (if (empty? a-lon)
      '()
      (local [(define SMALLER= (filter
                                (λ (i) (<= i (first a-lon)))
                                (rest a-lon)))
              (define GREATER  (filter
                                (λ (i) (> i (first a-lon)))
                                (rest a-lon)))]
        (append (quick-sorting SMALLER=)
                (cons (first a-lon)
                      (quick-sorting GREATER))))))

;; Sample expressions for quick-sorting
(define LON0-VAL '())
     
(define LON1-VAL
  (local
    [(define SMALLER= (filter
                       (λ (i) (<= i (first LON1)))
                       (rest LON1)))
     (define GREATER  (filter
                       (λ (i) (> i (first LON1)))
                       (rest LON1)))]
    (append (quick-sorting SMALLER=)
            (cons (first LON1)
                  (quick-sorting GREATER)))))
(define LON2-VAL
  (local
    [(define SMALLER= (filter
                       (λ(i) (<= i (first LON2)))
                       (rest LON2)))
     (define GREATER  (filter
                       (λ (i) (> i (first LON2)))
                       (rest LON2)))]
    (append (quick-sorting SMALLER=)
            (cons (first LON2)
                  (quick-sorting GREATER)))))
(define LON3-VAL
  (local
    [(define SMALLER= (filter
                       (λ (i) (<= i (first LON3)))
                       (rest LON3)))
     (define GREATER  (filter
                       (λ (i) (> i (first LON3)))
                       (rest LON3)))]
    (append (quick-sorting SMALLER=)
            (cons (first LON3)
                  (quick-sorting GREATER)))))

;; Tests using sample computations for quick-sorting
(check-expect (quick-sorting LON0) LON0-VAL)
(check-expect (quick-sorting LON1) LON1-VAL)
(check-expect (quick-sorting LON2) LON2-VAL)
(check-expect (quick-sorting LON3) LON3-VAL)

;; Tests using sample values for quick-sorting
(check-expect    (quick-sorting LON6) LON6)
(check-expect    (quick-sorting '(74 83 -72 2))
                 '(-72 2 74 83))

(define (endk v) v)

;; lon (lon --> lon) --> lon
;; Purpose: Sort given lon in nondecreasing order
;; How: When the given list is empty stop and return the
;;   empty list. Otherwise, place the given's list
;;   first number between the sorted numbers less than
;;   or equal to the first number and the sorted numbers
;;   greater than the first number.
(define (quick-sorting/k a-lon k)
  (if (empty? a-lon)
      (k '())
      (local [(define SMALLER= (filter
                                (λ (i) (<= i (first a-lon)))
                                (rest a-lon)))
              (define GREATER  (filter
                                (λ (i) (> i (first a-lon)))
                                (rest a-lon)))]
        (quick-sorting/k
         SMALLER=
         (λ (ssm) (quick-sorting/k
                   GREATER
                   (λ (sgr) (k (append ssm
                                       (cons (first a-lon) sgr))))))))))

;; Sample expressions for quick-sorting
(define LON0-VAL2 '())
     
(define LON1-VAL2
  (local [(define SMALLER= (filter
                                (λ (i) (<= i (first LON1)))
                                (rest LON1)))
              (define GREATER  (filter
                                (λ (i) (> i (first LON1)))
                                (rest LON1)))]
        (quick-sorting/k
         SMALLER=
         (λ (ssm) (quick-sorting/k
                   GREATER
                   (λ (sgr) (endk (append ssm
                                          (cons (first LON1) sgr)))))))))

(define LON2-VAL2
  (local [(define SMALLER= (filter
                                (λ (i) (<= i (first LON2)))
                                (rest LON2)))
              (define GREATER  (filter
                                (λ (i) (> i (first LON2)))
                                (rest LON2)))]
        (quick-sorting/k
         SMALLER=
         (λ (ssm) (quick-sorting/k
                   GREATER
                   (λ (sgr) (endk (append ssm
                                          (cons (first LON2) sgr)))))))))

(define LON3-VAL2
  (local [(define SMALLER= (filter
                                (λ (i) (<= i (first LON3)))
                                (rest LON3)))
              (define GREATER  (filter
                                (λ (i) (> i (first LON3)))
                                (rest LON3)))]
        (quick-sorting/k
         SMALLER=
         (λ (ssm) (quick-sorting/k
                   GREATER
                   (λ (sgr) (endk (append ssm
                                          (cons (first LON3) sgr)))))))))

;; Tests using sample computations for quick-sorting
(check-expect (quick-sorting LON0) LON0-VAL2)
(check-expect (quick-sorting LON1) LON1-VAL2)
(check-expect (quick-sorting LON2) LON2-VAL2)
(check-expect (quick-sorting LON3) LON3-VAL2)

;; Tests using sample values for quick-sorting
(check-expect    (quick-sorting LON6) LON6)
(check-expect    (quick-sorting '(74 83 -72 2))
                 '(-72 2 74 83))

(define L (build-list 100000 (λ (i) (random 1000000))))

(define T1 (time (quick-sorting L)))
(define T2 (time (quick-sorting/k L (λ (v) v))))
(define T3 (time (quick-sorting L)))
(define T4 (time (quick-sorting/k L (λ (v) v))))
(define T5 (time (quick-sorting L)))
(define T6 (time (quick-sorting/k L (λ (v) v))))
(define T7 (time (quick-sorting L)))
(define T8 (time (quick-sorting/k L (λ (v) v))))
(define T9 (time (quick-sorting L)))
(define T10 (time (quick-sorting/k L (λ (v) v))))
