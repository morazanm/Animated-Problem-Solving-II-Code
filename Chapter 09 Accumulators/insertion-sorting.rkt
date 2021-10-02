;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname insertion-sorting) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define LON0 '())
(define LON1 '(71 81 21 28 72 19 49 64 4 47 81 4))
(define LON2 '(91 57 93 5 16 56 61 59 93 49 -3))
(define LON3 (build-list 2500 (λ (i) (- 100000 i))))
(define LON4 (build-list 200 (λ (i) (random 100000000))))
(define LON5 (build-list 1575 (λ (i) (random 10000000))))
(define LON6 (build-list 1575 (λ (i) i)))
(define LON7 '(1 2 3 4 5))
(define LON8 '(-10 -5 0 5 10))


;; lon --> Boolean
;; Purpose: Determine if given list is sorted in nondecreasing order
(define (is-sorted? L)
  (local [;; lon --> Boolean
          ;; Purpose: Determine if given list is of length 1 or 2
          (define (short-lon? lst) (< (length lst) 2))]
    (or (short-lon? L)
        (and (<= (first L) (second L))
             (is-sorted? (rest L))))))

;; Tests using sample values for is-sorted?
(check-expect (is-sorted? '())      #true)
(check-expect (is-sorted? '(31))    #true)
(check-expect (is-sorted? '(3 1 9)) #false)
(check-expect (is-sorted? '(3 7 8)) #true)


;; insert: a-num lon ! lon
;; Purpose: To insert a num into a lon sorted in
;; non-decreasing order
(define (insert a-num a-lon)
  (cond [(empty? a-lon) (cons a-num empty)]
        [(<= a-num (first a-lon)) (cons a-num a-lon)]
        [else (cons (first a-lon)
                    (insert a-num (rest a-lon)))]))

;; sort: lon ! lon
;; Purpose: Sort given lon in nondecreasing order
(define (insertion-sorting a-lon)
  (cond [(empty? a-lon) '()]
        [else (insert (first a-lon)
                      (insertion-sorting (rest a-lon)))]))

;; Tests using sample values for insertion-sorting
(check-expect (insertion-sorting LON0) '())
(check-expect (insertion-sorting LON1)
              (list 4 4 19 21 28 47 49 64 71 72 81 81))
(check-expect (insertion-sorting LON2)
              (list -3 5 16 49 56 57 59 61 91 93 93))
(check-expect (insertion-sorting LON3) (reverse LON3))
(check-satisfied (insertion-sorting LON4) is-sorted?)
(check-satisfied (insertion-sorting LON5) is-sorted?)
(check-expect (insertion-sorting LON6) LON6)


;; number lon lon --> lon
;; Purpose: To insert the given number in the given sorted lon
;; Accumulator Invariant:
;;  accum = processed numbers in nondecreasing order &&
;;          (list-ref i accum) < a-num
;; Assumption: a-slon is sorted in nondecreasing order
(define (insert-accum a-num a-slon accum)
  (cond [(empty? a-slon) (append accum (list a-num))]
        [(<= a-num (first a-slon))
         (append accum (cons a-num a-slon))]
        [else (insert-accum a-num
                            (rest a-slon)
                            (append accum (list (first a-slon))))]))

;; Sample expressions for insert-accum
(define INSERTACC-LON0   (append '() (list 10)))
(define INSERTACC-EXMP   (append '(5 6 7) (list 20)))
(define INSERTACC-LON7   (append '() (cons 0 LON7)))
(define INSERTACC-LON8   (append '(-10 -5)
                                 (cons -2 (rest (rest LON8)))))
(define INSERTACC-LON7-2 (insert-accum
                          7
                          (rest LON7)
                          (append '() (list (first LON7)))))
(define INSERTACC-LON8-2 (insert-accum
                          8
                          (rest '(0 5 10))
                          (append '(-10 -5)
                                  (list (first '(0 5 10))))))

;; Tests using sample computations for insert-accum
(check-expect (insert-accum 10 '()  '())
              INSERTACC-LON0)
(check-expect (insert-accum 20 '()  '(5 6 7))
              INSERTACC-EXMP)
(check-expect (insert-accum  0 LON7 '())
              INSERTACC-LON7)
(check-expect (insert-accum -2
                            (rest (rest LON8))
                            '(-10 -5))
              INSERTACC-LON8)
(check-expect (insert-accum  7 LON7 '())
              INSERTACC-LON7-2)
(check-expect (insert-accum  8
                             (rest (rest LON8))
                             '(-10 -5))
              INSERTACC-LON8-2)

;; Tests using sample values for insert-accum
(check-expect (insert-accum 23 '()  '())
              '(23))
(check-expect (insert-accum 31 '(50 50)  '())
              '(31 50 50))
(check-expect (insert-accum 87 '(50 78 90)  '(20 30))
              '(20 30 50 78 87 90))


;; lon lon --> lon
;; Purpose: Sort the first given lon in nondecreasing order
;; Accumulator Invariant: accum = traversed elements in nondecreasing order
(define (insertion-sorting-accum a-lon accum)
  (cond [(empty? a-lon) accum]
        [else
         (insertion-sorting-accum (rest a-lon)
                                  (insert-accum (first a-lon) accum '()))]))

;; Sample expressions for insertion-sorting-accum
(define INSALON0-VAL '())
(define INSALON6-VAL LON6)
(define INSALON1-VAL (insertion-sorting-accum
                      (rest LON1)
                      (insert-accum (first LON1) '() '())))
(define INSALON4-VAL (insertion-sorting-accum
                      (rest LON4)
                      (insert-accum (first LON4) '() '())))

;; Tests using sample computations for insertion-sorting-accum
(check-expect (insertion-sorting-accum LON0 '())  INSALON0-VAL)
(check-expect (insertion-sorting-accum '()  LON6) INSALON6-VAL)
(check-expect (insertion-sorting-accum LON1 '())  INSALON1-VAL)
(check-expect (insertion-sorting-accum LON4 '())  INSALON4-VAL)

;; Tests using sample values for insertion-sorting-accum
(check-expect (insertion-sorting-accum '(3 1 8) '()) '(1 3 8))
(check-expect (insertion-sorting-accum '(3 1 8) '(1 8 9))
              '(1 1 3 8 8 9))


;; sort: lon --> lon
;; Purpose: Sort given lon in nondecreasing order
(define (insertion-sorting2 a-lon)
  (insertion-sorting-accum a-lon '()))

;; Sample expressions for insertion-sorting2
(define INS2LON0-VAL (insertion-sorting-accum LON0 '()))
(define INS2LON1-VAL (insertion-sorting-accum LON1 '()))
(define INS2LON2-VAL (insertion-sorting-accum LON2 '()))

;;  Tests using sample computations for insertion-sorting2
(check-expect (insertion-sorting2 LON0) INS2LON0-VAL)
(check-expect (insertion-sorting2 LON1) INS2LON1-VAL)
(check-expect (insertion-sorting2 LON2) INS2LON2-VAL)

;; Tests using sample values for insertion-sorting2
(check-expect    (insertion-sorting2 LON3) (reverse LON3))
(check-satisfied (insertion-sorting2 LON4) is-sorted?)
(check-satisfied (insertion-sorting2 LON5) is-sorted?)
(check-expect    (insertion-sorting LON6)  LON6)




;(define T1 (time (insertion-sorting LON3)))
;(define T2 (time (insertion-sorting2 LON3)))
(define T1 (time (insertion-sorting LON5)))
(define T2 (time (insertion-sorting2 LON5)))
(define T3 (time (insertion-sorting LON5)))
(define T4 (time (insertion-sorting2 LON5)))
(define T5 (time (insertion-sorting LON5)))
(define T6 (time (insertion-sorting2 LON5)))
(define T7 (time (insertion-sorting LON5)))
(define T8 (time (insertion-sorting2 LON5)))
(define T9 (time (insertion-sorting LON5)))
(define T10 (time (insertion-sorting2 LON5)))