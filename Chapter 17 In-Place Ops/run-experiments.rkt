;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname run-experiments) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define V0 (vector))
(define V1 (vector 10 3 7 17 11))
(define V2 (vector 31 46 60 22 74 22 27 60 20 44 23 85 86 67 12 75 80 77 62 37))
(define V3 (build-vector 10 (λ (i) i)))
(define V4 (build-vector 10 (λ (i) (sub1 (- 10 i)))))

;; (vectorof number) --> (void)
;; Purpose: Sort the given vector in nondecreasing order
;; Effect: Given vector elements are rearranged in place
(define (qs-in-place! V)
  (local [;; natnum natnum --> (void)
          ;; Purpose: Swap the elements at the given indices
          ;; Effect: V is mutated by swapping elements at given indices
          (define (swap! i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))

          ;; number [int int] --> natnum
          ;; Purpose: Find first index <= high, i, such that V[i] <= given pivot
          ;; Assumption: V[low] = pivot
          (define (find<= pivot low high)
            (if (or (> low high)
                    (<= (vector-ref V high) pivot))
                high
                (find<= pivot low (sub1 high))))

          ;; number [int int] --> natnum
          ;; Purpose: Find first index >= low, i, such that V[i] > given pivot
          ;;          if it exists. Otherwise, return high + 1
          (define (find> pivot low high)
            (if (or (> low high)
                    (> (vector-ref V low) pivot))
                low
                (find> pivot (add1 low) high)))
          
          ;; number [int int] --> natnum
          ;; Purpose: Return the position of the pivot in the sorted V
          ;; How: The smallest index of a vector element > pivot and the largest
          ;;      index of an element <= to the pivot are found. If they form an
          ;;      empty vector interval the largest index of an element <= to the
          ;;      pivot is returned as the position of the pivot in the sorted
          ;;      vector. Otherwise, the two indexed values are swapped and the
          ;;      partitioning process continues with the vector interval between
          ;;      the two indices.
          ;; Effect: V's elements are rearranged so that all elements before and including
          ;;         the pivot position in sorted V are <= to the pivot and all elements
          ;;         after the pivot position in sorted V are > to the pivot
          (define (partition! pivot low high)
            (local [(define first>pivot (find> pivot low high))
                    (define first<=pivot (find<= pivot low high))]
              (if (> first>pivot first<=pivot)
                  first<=pivot
                  (begin
                    (swap! first>pivot first<=pivot)
                    (partition! pivot
                                first>pivot
                                first<=pivot)))))
          ;; Termination Argument
          ;; Every recursive call is made with a smaller vector interval
          ;; that does not contain the beginning numbers <= to the pivot
          ;; and the ending numbers > pivot. Eventually, the recursive call
          ;; is made with an empty vector interval and the function halts.
          

          ;; [int int] --> (void)
          ;; Purpose: Sort V's elements in the given vector interval in nondecreasing order
          ;; How: The vector is partitioned in two. The first element is placed in the vector
          ;;      position between the elements <= to it and the elements > than it. The
          ;;      The vector intervals for the two parts of the partition are recursively sorted
          ;; Effect: Vector elements in the given vector interval are sorted in place
          (define (qs-aux! low high)
            (if (> low high)
                (void)
                (local [(define pivot (vector-ref V low))
                        (define pivot-pos (partition! pivot low high))]
                  (begin
                    (swap! low pivot-pos)
                    (qs-aux! low (sub1 pivot-pos))
                    (qs-aux! (add1 pivot-pos) high)))))
          ;; Termination Argument
          ;; A given nonempty vector interval is divided into two smaller vector
          ;; intervals and these are recursively processed. Eventually, the given
          ;; vector interval is empty and the function halts.
                  
          ]
    (qs-aux! 0 (sub1 (vector-length V)))))

(check-expect (begin
                (qs-in-place! V0)
                V0)
              (vector))

(check-expect (begin
                (qs-in-place! V1)
                V1)
              (vector 3 7 10 11 17))

(check-expect (begin
                (qs-in-place! V2)
                V2)
              (vector 12 20 22 22 23 27 31 37 44 46 60 60 62 67 74 75 77 80 85 86))

(check-expect (begin
                (qs-in-place! V3)
                V3)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (qs-in-place! V4)
                V4)
              (vector 0 1 2 3 4 5 6 7 8 9))

(define (heap-sort-in-place! V)
  (local [;; natnum natnum --> (void)
          ;; Purpose: Swap the elements at the given indices
          ;; Effect: V is mutated by swapping elements at given indices
          (define (swap! i j)
            (local [(define temp (vector-ref V i))]
              (begin
                (vector-set! V i (vector-ref V j))
                (vector-set! V j temp))))

          ;; natnum --> natnum
          ;; Purpose: Return the index for the right subheap's root
          (define (right-heap-root parent-index)
            (+ (* 2 parent-index) 2))
                    
          ;; natnum --> natnum
          ;; Purpose: Return the index for the left subheap's root
          (define (left-heap-root parent-index)
            (add1 (* 2 parent-index)))

          ;; natnum --> natnum
          ;; Purpose: Return the parent index in the heap
          ;; Assumption: the given index is in [0..(sub1 (vector-length V))]
          (define (parent i)
            (if (even? i)
                (quotient (sub1 i) 2)
                (quotient i 2)))
                           
          ;; heapify!: [int int] --> (void)
          ;; Purpose: For the given VINTV, make the given vector a heap
          ;; Effect: Reorganizes the vector elements to form a heap rooted at low
          ;; Assumption: The given VINTV is valid for V and low > 0
          (define (heapify! low high)
            (cond [(> low high) (void)]
                  [else
                   (local [(define parent-index (parent high))]
                     (cond [(>= (vector-ref V parent-index) (vector-ref V high))
                            (heapify! low (sub1 high))]
                           [else (begin
                                   (swap! parent-index high)
                                   (trickle-down! high (sub1 (vector-length V)))
                                   (heapify! low (sub1 high)))]))]))

          ;; trickle-down!: [int int] --> (void)
          ;; Purpose: For the given VINTV, re-establish a heap rooted at low
          ;; Effect: Vector elements are rearranged to have a heap rooted at low
          ;; Assumption: V[low+1..high] are all heap roots
          (define (trickle-down! low high)
            (local [(define rc-index (right-heap-root low))
                    (define lc-index (left-heap-root low))]
              (cond [(> lc-index high) (void)] ;; root has no children
                    [(> rc-index high)         ;; root only has a left child
                     (if (<= (vector-ref V lc-index) (vector-ref V low))
                         (void)
                         (begin
                           (swap! low lc-index)
                           (trickle-down! lc-index high)))]
                    [else ;; root has two children
                     (local [(define mc-index (if (>= (vector-ref V lc-index)
                                                      (vector-ref V rc-index))
                                                  lc-index
                                                  rc-index))]
                       (cond [(>= (vector-ref V low) (vector-ref V mc-index))
                              (void)]
                             [else (begin
                                     (swap! low mc-index)
                                     (trickle-down! mc-index high))]))])))
          ;; Termination argument
          ;; A recursive call is only made when V[low] has 1 or 2 children.
          ;; That is, the index of any child must be less than or equal to
          ;; high. Every recursive call is made with an interval formed by
          ;; a child index and high. Eventually, the given vector interval
          ;; becomes empty or V[low} is a heap root and the mutator terminates.
    
          ;; [int int] --> (void)
          ;; Purpose: For the given VINTV, sort the vector elements
          ;; Effect: V's elements in the VINTV are rearranged in non-decreasing order
          ;; Assumption: V is a heap and given VINTV is valid for V
          (define (sorter! low high)
            (cond [(> low high) (void)]
                  [else (begin
                          (swap! low high)
                          (trickle-down! low (sub1 high))
                          (sorter! low (sub1 high)))]))]
    (begin
      (heapify! 1 (sub1 (vector-length V)))
      (sorter! 0 (sub1 (vector-length V))))))

(check-expect (begin
                (heap-sort-in-place! V0)
                V0)
              (vector))

(check-expect (begin
                (heap-sort-in-place! V1)
                V1)
              (vector 3 7 10 11 17))

(check-expect (begin
                (heap-sort-in-place! V2)
                V2)
              (vector 12 20 22 22 23 27 31 37 44 46 60 60 62 67 74 75 77 80 85 86))

(check-expect (begin
                (heap-sort-in-place! V3)
                V3)
              (vector 0 1 2 3 4 5 6 7 8 9))

(check-expect (begin
                (heap-sort-in-place! V4)
                V4)
              (vector 0 1 2 3 4 5 6 7 8 9))


;; natnum (listof ((vectorof number) --> (void))) --> (void)
;; Purpose: Run empirical study with vectors that have lengths
;;          that are multiples of 1000 in [1000..natnum*1000]
(define (empirical-study factor lst-of-sorters)
  (local [(define NUM-RUNS 5)
          
          (define V (build-vector (* factor 1000) (λ (i) (random 10000000))))

          ;; (vectorof number) natnum (litsof ((vectorof number) --> (void)))--> (void)
          ;; Purpose: Time the given in-place sorters using the  given vector
          ;;          the given number of times
          (define (run-experiments V runs sorters)
            (local [;; natnum --> (void)
                    ;; Purpose: Run n experiments
                    (define (run sorters)
                      (if (empty? sorters)
                          (void)
                          (local [;; (vectorof number)
                                  ;; Purpose: Copy of V to sort
                                  (define V1 (build-vector (vector-length V)
                                                           (λ (i) (vector-ref V i))))]
                            (begin
                              (display (format "Sorter ~s: "
                                               (add1 (- (length lst-of-sorters)
                                                        (length sorters)))))
                              (time ((first sorters) V1))
                              (run (rest sorters))))))]
              (if (= runs 0)
                  (void)
                  (begin
                    (display (format "     RUN ~s\n" (add1 (- NUM-RUNS runs))))
                    (run sorters)
                    (run-experiments V (sub1 runs) sorters)))))]
    (if (= factor 0)
        (void)
        (begin
          (display (format "Experiments for length ~s \n" (* factor 1000)))
          (run-experiments V NUM-RUNS lst-of-sorters)
          (newline)
          (newline)
          (empirical-study (sub1 factor) lst-of-sorters)))))

(empirical-study 100 (list qs-in-place! heap-sort-in-place!))