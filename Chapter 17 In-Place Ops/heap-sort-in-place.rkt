;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname heap-sort-in-place) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

#|
A heap is either:
  1. empty
  2. (list number heap heap), where the number is
     greater than the roots, if any, of the two subheaps

Template for functions on a heap

;; Sample heaps
(define HEAP0 '())
(define HEAP1 ...)
     ...
  
;; heap ... --> ...
;; Purpose:
(define (f-on-heap a-heap ...)
  (if (empty? a-heap)
      ...
      ...(first a-heap)...
         (f-on-heap (second a-heap)...
         (f-on-heap (third a-heap)...))

;; Sample expressions for f-on-heap
(define HEAP0-VAL ...)
(define HEAP1-VAL ...)
     ...

;; Tests using sample computations for f-on-heap
(check-expect (f-on-heap HEAP0 ...) HEAP0-VAL)
(check-expect (f-on-heap HEAP1 ...) HEAP1-VAL)
     ...

;; Tests using sample values for f-on-heap
(check-expect (f-on-heap ... ...) ...)
     ...

|#


(define V0 (vector))
(define V1 (vector 10 3 7 17 11))
(define V2 (vector 31 46 60 22 74 22 27 60 20 44 23 85 86 67 12 75 80 77 62 37))
(define V3 (build-vector 10 (λ (i) i)))
(define V4 (build-vector 10 (λ (i) (sub1 (- 10 i)))))

;; (vectorof number) --> (void)
;; Purpose: Sort the given vector in nondecreasing order
;; Effect: The given vector's elements are rearranged in nondecreasing order
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