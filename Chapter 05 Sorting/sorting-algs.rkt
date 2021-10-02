;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sorting-algs) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define LON0 '())
(define LON1 '(71 81 21 28 72 19 49 64 4 47 81 4))
(define LON2 '(91 57 93 5 16 56 61 59 93 49 -3))
(define LON3 (build-list 2500 (λ (i) (- 100000 i))))
(define LON4 (build-list 500  (λ (i) (random 100000000))))
(define LON5 (build-list 325  (λ (i) (random 10000000))))
(define LON6 (build-list 1575 (λ (i) i)))

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


; sort: lon --> lon
; Purpose: Sort given lon in nondecreasing order
(define (insertion-sorting a-lon)
  (local [; insert: a-num lon --> lon
          ; Purpose: To insert a num into a lon sorted in non-decreasing order
          (define (insert a-num a-lon)
            (cond 	[(empty? a-lon) (cons a-num empty)]
                        [(<= a-num (first a-lon)) (cons a-num a-lon)]
                        [else (cons (first a-lon) 
                                    (insert a-num (rest a-lon)))]))]
    (cond 	[(empty? a-lon) empty]
                [else (insert (first a-lon) (insertion-sorting (rest a-lon)))])))

;; Tests using sample values for insertion-sorting
(check-expect (insertion-sorting LON0) '())
(check-expect (insertion-sorting LON1) (list 4 4 19 21 28 47 49 64 71 72 81 81))
(check-expect (insertion-sorting LON2) (list -3 5 16 49 56 57 59 61 91 93 93))
(check-expect (insertion-sorting LON3) (reverse LON3))
(check-satisfied (insertion-sorting LON4) is-sorted?)
(check-satisfied (insertion-sorting LON5) is-sorted?)
(check-expect (insertion-sorting LON6) LON6)


;; sort: lon --> lon
;; Purpose: Sort given lon in nondecreasing order
;; How: When the give list is empty stop and return the
;;   empty list. Otherwise, place the given's list
;;   first number between the sorted numbers less than
;;   or equal to the first number and the sorted numbers
;;   greater than the first number.
(define (quick-sorting a-lon)
  (if (empty? a-lon) ;; O(n)
      '()
      ;; splitting O(lg n), append O(n)
      (local [(define SMALLER= (filter (λ (i) (<= i (first a-lon)))
                                       (rest a-lon)))
              (define GREATER  (filter (λ (i) (> i (first a-lon)))
                                       (rest a-lon)))]
        (append (quick-sorting SMALLER=)
                (cons (first a-lon)
                      (quick-sorting GREATER))))))

;; Sample expressions for quick-sorting
(define QS-LON0-VAL '())
(define QS-LON1-VAL (local [(define SMALLER= (filter (λ (i) (<= i (first LON1)))
                                                     (rest LON1)))
                            (define GREATER  (filter (λ (i) (> i (first LON1)))
                                                     (rest LON1)))]
                      (append (quick-sorting SMALLER=)
                              (cons (first LON1)
                                    (quick-sorting GREATER)))))
(define QS-LON2-VAL (local [(define SMALLER= (filter (λ (i) (<= i (first LON2)))
                                                     (rest LON2)))
                            (define GREATER  (filter (λ (i) (> i (first LON2)))
                                                     (rest LON2)))]
                      (append (quick-sorting SMALLER=)
                              (cons (first LON2)
                                    (quick-sorting GREATER)))))
(define QS-LON3-VAL (local [(define SMALLER= (filter (λ (i) (<= i (first LON3)))
                                                     (rest LON3)))
                            (define GREATER  (filter (λ (i) (> i (first LON3)))
                                                     (rest LON3)))]
                      (append (quick-sorting SMALLER=)
                              (cons (first LON3)
                                    (quick-sorting GREATER)))))

;; Tests using sample computations for quick-sorting
(check-expect (quick-sorting LON0) QS-LON0-VAL)
(check-expect (quick-sorting LON1) QS-LON1-VAL)
(check-expect (quick-sorting LON2) QS-LON2-VAL)
(check-expect (quick-sorting LON3) QS-LON3-VAL)

;; Tests using sample values for quick-sorting
(check-satisfied (quick-sorting LON4) is-sorted?)
(check-satisfied (quick-sorting LON5) is-sorted?)
(check-expect    (quick-sorting LON6) LON6)
(check-expect    (quick-sorting '(74 83 -72 2))
                 '(-72 2 74 83))

(define SL1 '())
(define SL2 '(-98 -76 -8 -1))
(define SL3 '(-87 -28 -6 89))
(define SL4 '(6 7 31 87))

;; lon lon --> lon
;; Purpose: Merge the given lons in nondecreasing order
;; Assumption: Given lons are in nondecreasing order
(define (merge l1 l2)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [(<= (first l1) (first l2))
         (cons (first l1) (merge (rest l1) l2))]
        [else (cons (first l2) (merge l1 (rest l2)))]))

;; Sample expressions for merge
(define M-SL1-SL2-VAL SL2)
(define M-SL1-SL3-VAL SL3)
(define M-SL2-SL1-VAL SL2)
(define M-SL3-SL1-VAL SL3)
(define M-SL2-SL3-VAL (cons (first SL2) (merge (rest SL2) SL3)))
(define M-SL3-SL4-VAL (cons (first SL3) (merge (rest SL3) SL4)))
(define M-SL4-SL3-VAL (cons (first SL3) (merge SL4 (rest SL3))))
(define M-SL3-SL2-VAL (cons (first SL2) (merge SL3 (rest SL2))))

;; Tests using sample computations for merge
(check-expect (merge SL1 SL2) M-SL1-SL2-VAL)
(check-expect (merge SL1 SL3) M-SL1-SL3-VAL)
(check-expect (merge SL2 SL1) M-SL2-SL1-VAL)
(check-expect (merge SL3 SL1) M-SL3-SL1-VAL)
(check-expect (merge SL2 SL3) M-SL2-SL3-VAL)
(check-expect (merge SL3 SL4) M-SL3-SL4-VAL)
(check-expect (merge SL4 SL3) M-SL4-SL3-VAL)
(check-expect (merge SL3 SL2) M-SL3-SL2-VAL)

;; Tests using sample values for merge
(check-expect (merge '() '()) '())
(check-expect (merge '() '(7 8 9)) '(7 8 9))
(check-expect (merge '(78 98) '()) '(78 98))
(check-expect (merge '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-expect (merge '(0 88) '(-5 8 17)) '(-5 0 8 17 88))

(define LOLON1 (list (list 1 2 3 4)))
(define LOLON2 (list (list -6 8 10 67)))
(define LOLON3 (list (list 1 2 3) (list -4 0 8 74) (list 5)))
(define LOLON4 (list (list 76 89 99) (list -77) (list 5 8 9)))

;; (listof lon) --> (listof lon)
;; Purpose: Merge every two adjacent lons in nondecreasing order
;; How: If the given (listof lon) has a length less
;;      than 2 there are no lons to merge and the
;;      answer is the given (listof lon). Otherwise,
;;      the merging of the first two lons is added
;;      to the front of the result of processing
;;      all the remaining lons after the first two.
;; Assumption: Nested lons are in nondecreasing order
(define (merge-neighs a-lolon)
  (if (< (length a-lolon) 2)
      a-lolon
      (local [(define NEW-LOLON (rest (rest a-lolon)))]
        (cons (merge (first a-lolon) (second a-lolon))
              (merge-neighs NEW-LOLON)))))

;; Sample expressions for merge-neighs
(define MN-LOLON1-VAL LOLON1)
(define MN-LOLON2-VAL LOLON2)
(define MN-LOLON3-VAL (local [(define NEW-LOLON (rest (rest LOLON3)))]
                        (cons (merge (first LOLON3) (second LOLON3))
                              (merge-neighs NEW-LOLON))))
(define MN-LOLON4-VAL (local [(define NEW-LOLON (rest (rest LOLON4)))]
                        (cons (merge (first LOLON4) (second LOLON4))
                              (merge-neighs NEW-LOLON))))

;; Tests using sample computations for merge-neighs
(check-expect (merge-neighs LOLON1) MN-LOLON1-VAL)
(check-expect (merge-neighs LOLON2) MN-LOLON2-VAL)
(check-expect (merge-neighs LOLON3) MN-LOLON3-VAL)
(check-expect (merge-neighs LOLON4) MN-LOLON4-VAL)

;; Tests using sample values for merge-neighs
(check-expect (merge-neighs '((5 8) (7 9) (-3)))
              '((5 7 8 9) (-3)))
(check-expect (merge-neighs '((2) (-9) (-3) (8 10)))
              '((-9 2) (-3 8 10)))

;; (listof lon) --> (listof lon)
;; Purpose: Sort the numbers in the given (listof lon)
;; How: If the length of the given (listof lon) is 1 return it.
;;      Otherwise, every two neighboring lons are merged to
;;      create a new problem instance that is recursively
;;      processed.
;; Assumption: The given (listof lon) has a length greater or equal to 1
(define (merge-sort-helper a-lolon)
  (if (= (length a-lolon) 1)
      a-lolon
      (local [(define NEW-LOLON (merge-neighs a-lolon))]
        (merge-sort-helper NEW-LOLON))))

;; Sample expressions for merge-sort-helper
(define MSH-LOLON1-VAL LOLON1)
(define MSH-LOLON2-VAL LOLON2)
(define MSH-LOLON3-VAL (local [(define NEW-LOLON (merge-neighs LOLON3))]
                         (merge-sort-helper NEW-LOLON)))
(define MSH-LOLON4-VAL (local [(define NEW-LOLON (merge-neighs LOLON4))]
                         (merge-sort-helper NEW-LOLON)))

;; Tests using sample computations for merge-sort-helper
(check-expect (merge-sort-helper LOLON1) MSH-LOLON1-VAL)
(check-expect (merge-sort-helper LOLON2) MSH-LOLON2-VAL)
(check-expect (merge-sort-helper LOLON3) MSH-LOLON3-VAL)
(check-expect (merge-sort-helper LOLON4) MSH-LOLON4-VAL)

;; Tests using sample values for merge-sort-helper
(check-expect (merge-sort-helper '((8) (7) (4)))
              '((4 7 8)))
(check-expect (merge-sort-helper '((8 9) (-87) (-4 99 678)))
              '((-87 -4 8 9 99 678)))

;; lon --> lon
;; Purpose: Sort given lon in nondecreasing order
(define (merge-sorting a-lon)
  (local [;; lon lon --> lon
          ;; Purpose: Merge the given lons in nondecreasing order
          ;; Assumption: Given lons are in nondecreasing order
          (define (merge l1 l2)
            (cond [(empty? l1) l2]
                  [(empty? l2) l1]
                  [(<= (first l1) (first l2))
                   (cons (first l1) (merge (rest l1) l2))]
                  [else (cons (first l2) (merge l1 (rest l2)))]))
          ;; (listof lon) --> (listof lon)
          ;; Purpose: Merge every two adjacent lons in nondecreasing order
          ;; How: If the given (listof lon) has a length less
          ;;      than 2 there are no lons to merge and the
          ;;      answer is the given (listof lon). Otherwise,
          ;;      the merging of the first two lons is added
          ;;      to the front of the result of processing
          ;;      all the remaining lons after the first two.
          ;; Assumption: Nested lons are in nondecreasing order
          (define (merge-neighs a-lolon)
            (if (< (length a-lolon) 2)
                a-lolon
                (local [(define NEW-LOLON (rest (rest a-lolon)))]
                  (cons (merge (first a-lolon) (second a-lolon))
                        (merge-neighs NEW-LOLON)))))
          ;; (listof lon) --> (listof lon)
          ;; Purpose: Sort the numbers in the given (listof lon)
          ;; How: If the length of the given (listof lon) is 1 return it.
          ;;      Otherwise, every two neighboring lons are merged to
          ;;      create a new problem instance that is recursively
          ;;      processed.
          ;; Assumption: The given (listof lon) has a length greater or equal to 1
          (define (merge-sort-helper a-lolon)
            (if (= (length a-lolon) 1)
                a-lolon
                (local [(define NEW-LOLON (merge-neighs a-lolon))]
                  (merge-sort-helper NEW-LOLON))))]
    (if (empty? a-lon)
        '()
        (first (merge-sort-helper (map (λ (n) (list n)) a-lon))))))

;; Sample expressions for merge-sorting
(define MS-LON0-VAL '())
(define MS-LON1-VAL (first (merge-sort-helper (map (λ (n) (list n)) LON1))))
(define MS-LON2-VAL (first (merge-sort-helper (map (λ (n) (list n)) LON2))))
(define MS-LON3-VAL (first (merge-sort-helper (map (λ (n) (list n)) LON3))))

;; Tests using sample values for merge-sorting
(check-expect (merge-sorting LON0) MS-LON0-VAL)
(check-expect (merge-sorting LON1) MS-LON1-VAL)
(check-expect (merge-sorting LON2) MS-LON2-VAL)
(check-expect (merge-sorting LON3) MS-LON3-VAL)

;; Tests using sample values for merge-sorting
(check-satisfied (merge-sorting LON4) is-sorted?)
(check-satisfied (merge-sorting LON5) is-sorted?)
(check-expect    (merge-sorting LON6) LON6)
(check-expect    (merge-sorting '(74 83 -72 2))
                 '(-72 2 74 83))
                    

; sort: lon --> lon
; Purpose: Sort given lon in nondecreasing order
(define (heap-sorting a-lon)
  (local [;; heap --> lon
          ;; Purpose: Return a lon in nondecreasing order
          (define (sort a-heap)
            (if (or (empty? a-heap) (empty? (rest a-heap)))
                a-heap
                (cons (first a-heap) (sort (heapify (rest a-heap))))))

          ;; lon --> hon
          ;; Purpose: Convert the given lon into a lon
          (define (heapify a-lon)
            (local [;; [int int] --> heap
                    ;; Purpose: Convert a-lon into a heap
                    (define (make-heap lst lw hg)
                      (local [;; [int..int] --> (listof X)
                              ;; Purpose: Return the list with the elements in the given interval into given list
                              (define (take L low high) ;; O(n)
                                (if (< high low)
                                    '()
                                    (cons (list-ref L low)
                                          (take L (add1 low) high))))

                              ;; (listof X) natnum natnum --> (listof X)
                              ;; Purpose: Swap values in given list at the given positions
                              ;; Assumption: Given positions are valid for given list and i < j
                              (define (swap-in-list L i j) ;; O(n)
                                (if (= i j)
                                    L
                                    (append (take L 0 (sub1 i))
                                            (list (list-ref L j))
                                            (take L (add1 i) (sub1 j))
                                            (list (list-ref L i))
                                            (take L (add1 j) (sub1 (length L))))))]
                        (if (< hg lw) ;; n steps
                            lst
                            (local [(define parent (quotient hg 2))]
                              (if (<= (list-ref lst parent) (list-ref lst hg))
                                  (make-heap lst lw (sub1 hg))
                                  (local [(define swapped-lst (swap-in-list lst parent hg))] ;; n steps
                                    (make-heap (append (take swapped-lst 0 (sub1 parent)) ;; append n steps, take n steps
                                                       (heapify (take swapped-lst parent (sub1 (length swapped-lst)))));; take n steps, heapify lg n steps
                                               lw
                                               (sub1 hg))))))))]
              (make-heap a-lon 0 (sub1 (length a-lon)))))]
    (if (empty? a-lon)
        '()
        (sort (heapify a-lon)))))

;; Tests using sample values for quick-sorting
;(check-expect (heap-sorting LON0) '())
;(check-expect (heap-sorting LON1) (list 4 4 19 21 28 47 49 64 71 72 81 81))
;(check-expect (heap-sorting LON2) (list 5 16 49 56 57 59 61 91 93 93))
;(check-expect (merge-sorting LON3) (reverse LON3))
;(check-satisfied (merge-sorting LON4) is-sorted?)
;(check-satisfied (merge-sorting LON5) is-sorted?)

; sort: lon --> lon
; Purpose: Sort given lon in nondecreasing order
(define (bubble-sorting a-lon)
  (local [;; lon --> lon
          ;; Purpose: Bubble the largest number to the end to the end of the given list
          (define (bubble a-lon)
            (cond [(or (empty? a-lon)
                       (empty? (rest a-lon)))
                   a-lon]
                  [(<= (first a-lon) (second a-lon))
                   (cons (first a-lon) (bubble (rest a-lon)))]
                  [else (cons (second a-lon) (bubble (cons (first a-lon) (rest (rest a-lon)))))]))
          ;; lon natnum --> lon
          ;;Purpose: Sort the given lon
          (define (sort a-lon n)
            (if (<= n 0)
                a-lon
                (sort (bubble a-lon) (sub1 n))))]
    (sort a-lon (sub1 (length a-lon)))))

;; Tests using sample values for insertion-sorting
(check-expect (bubble-sorting LON0) '())
(check-expect (bubble-sorting LON1) (list 4 4 19 21 28 47 49 64 71 72 81 81))
(check-expect (bubble-sorting LON2) (list -3 5 16 49 56 57 59 61 91 93 93))
(check-expect (bubble-sorting LON3) (reverse LON3))
(check-satisfied (bubble-sorting LON4) is-sorted?)
(check-satisfied (bubble-sorting LON5) is-sorted?)
(check-expect (bubble-sorting LON6) LON6)

"LON1 Timing"
(define T-ins-LON1 (time (insertion-sorting LON1)))
(define T-qs-LON1 (time (quick-sorting LON1)))
(define T-ms-LON1 (time (merge-sorting LON1)))
"LON2 Timing"
(define T-ins-LON2 (time (insertion-sorting LON2)))
(define T-qs-LON2 (time (quick-sorting LON2)))
(define T-ms-LON2 (time (merge-sorting LON2)))
"LON3 Timing"
(define T-ins-LON3 (time (insertion-sorting LON3)))
(define T-qs-LON3 (time (quick-sorting LON3)))
(define T-ms-LON3 (time (merge-sorting LON3)))
"LON4 Timing"
(define T-ins-LON4 (time (insertion-sorting LON4)))
(define T-qs-LON4 (time (quick-sorting LON4)))
(define T-ms-LON4 (time (merge-sorting LON4)))
"LON5 Timing"
(define T-ins-LON5 (time (insertion-sorting LON5)))
(define T-qs-LON5 (time (quick-sorting LON5)))
(define T-ms-LON5 (time (merge-sorting LON5)))
"LON6 Timing"
(define T-ins-LON6 (time (insertion-sorting LON6)))
(define T-qs-LON6 (time (quick-sorting LON6)))
(define T-ms-LON6 (time (merge-sorting LON6)))
"-----"


;(define T4 (time (heap-sorting LON3)))