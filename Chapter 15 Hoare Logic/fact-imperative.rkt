;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname fact-imperative) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(require while)


;;; natnum --> natnum
;;; Purpose: Compute the factorial of the given natural number
;(define (fact2-v1 n)
;  (local [;; natnum
;          ;; Purpose: The next possible accum factor
;          (define k (void))
;
;          ;; natnum
;          ;; Purpose: The current approximation of n!
;          (define accum (void))
;          
;          ;; natnum natnum --> natnum
;          ;; Purpose: Compute the factorial of the first
;          ;;          given natural number
;          ;; Effect: k is decremented and
;          ;;         accum is multiplied by k
;          ;; Accumulator invariant
;          ;;   accum = the product of the natural numbers in [k+1..n]
;          (define (fact-state!)
;            (if (= k 0)
;                accum
;                (begin
;                  (set! k (sub1 k))
;                  (set! accum (* accum k))
;                  (fact-state!))))]
;    (begin
;      (set! k n)
;      (set! accum 1)
;      (fact-state!))))
;
;;; Tests using sample values for fact2-v1
;(check-expect (fact2-v1 0)  1)
;(check-expect (fact2-v1 5)  120)
;(check-expect (fact2-v1 10) 3628800)
;(check-expect (fact2-v1 2)  2)
;(check-expect (fact2-v1 6)  720)


;;; natnum --> natnum
;;; Purpose: Compute the factorial of the given natural number
;(define (fact2-v2 n)
;  (local [;; natnum
;          ;; Purpose: The next possible accum factor
;          (define k (void))
;
;          ;; natnum
;          ;; Purpose: The current approximation of n!
;          (define accum (void))
;          
;          ;; natnum natnum --> natnum
;          ;; Purpose: Compute the factorial of the first
;          ;;          given natural number
;          ;; Effect: k is decremented and
;          ;;         accum is multiplied by k
;          ;; Accumulator invariant
;          ;;   accum = the product of the natural numbers in [k+1..n]
;          (define (fact-state!)
;            (if (= k 0)
;                accum
;                (begin
;                  (set! accum (* accum k))
;                  (set! k (sub1 k))
;                  (fact-state!))))]
;    (begin
;      (set! k n)
;      (set! accum 1)
;      (fact-state!))))
;
;;; Tests using sample values for fact2-v2
;(check-expect (fact2-v2 0)  1)
;(check-expect (fact2-v2 5)  120)
;(check-expect (fact2-v2 10) 3628800)
;(check-expect (fact2-v2 2)  2)
;(check-expect (fact2-v2 6)  720)

#|
;; natnum --> natnum
;; Purpose: Compute the factorial of the given natural number
(define (fact2-v3 n)
  (local [;; natnum
          ;; Purpose: The next possible accum factor
          (define k (void))

          ;; natnum
          ;; Purpose: The current approximation of n!
          (define accum (void))
          
          ;; natnum natnum --> natnum
          ;; Purpose: Compute the factorial of the first
          ;;          given natural number
          ;; Effect: k is decremented and
          ;;         accum is multiplied by k
          ;; Accumulator invariant
          ;;   accum = the product of the natural numbers in [k+1..n]
          (define (fact-state!)
            ;; accum = \(\Pi\sb\texttt{i=k+1}\sp\texttt{n} \texttt{i}\)
            (if (= k 0)
                ;; accum = \(\Pi\sb\texttt{i=k+1}\sp\texttt{n} \texttt{i}\) \logand k = 0
                ;; \(\Rightarrow\) accum = \(\Pi\sb\texttt{i=1}\sp\texttt{n} \texttt{i}\)
                ;; \(\Rightarrow\) accum = n!
                accum
                ;; else branch
                (begin
                  ;; accum = \(\Pi\sb\texttt{i=k+1}\sp\texttt{n} \texttt{i}\) \logand k \(\neq\) 0
                  (set! accum (* k accum))
                  ;; accum = \(\Pi\sb\texttt{i=k}\sp\texttt{n} \texttt{i}\)
                  (set! k (sub1 k))
                  ;; accum = \(\Pi\sb\texttt{i=k+1}\sp\texttt{n} \texttt{i}\)
                  (fact-state!))))
          ;; Termination argument:
          ;; \texttt{k} is a natural number that is decremented with each recursive call.
          ;; Eventually, \texttt{k} becomes 0 and the recursion stops.
          ]
    (begin
      (set! k n)
      (set! accum 1)
      (fact-state!))))

;; Tests using sample values for fact2-v3
(check-expect (fact2-v3 0)  1)
(check-expect (fact2-v3 5)  120)
(check-expect (fact2-v3 10) 3628800)
(check-expect (fact2-v3 2)  2)
(check-expect (fact2-v3 6)  720)

|#

;;; natnum --> natnum
;;; Purpose: Compute the factorial of the given natural number
;(define (fact2-v4 n)
;  (local [;; natnum
;          ;; Purpose: The next possible accum factor
;          (define k (void))
;
;          ;; natnum
;          ;; Purpose: The current approximation of n!
;          (define accum (void))
;          
;          ;; natnum natnum --> natnum
;          ;; Purpose: Compute the factorial of the first
;          ;;          given natural number
;          ;; Effect: k is decremented and
;          ;;         accum is multiplied by k
;          ;; Accumulator invariant
;          ;;   accum = the product of the natural numbers in [k+1..n]
;          (define (fact-state!)
;            (begin
;              (set! k n)
;              (set! accum 1)
;              ;; accum = \(\Pi\sb\texttt{i=k+1}\sp\texttt{n} \texttt{i}\)
;              (while (not (= k 0))
;                    ;; accum = \(\Pi\sb\texttt{i=k+1}\sp\texttt{n} \texttt{i}\) \logand k \(\neq\) 0
;                    (set! accum (* k accum))
;                    ;; accum = \(\Pi\sb\texttt{i=k}\sp\texttt{n} \texttt{i}\)
;                    (set! k (sub1 k))
;                    ;; accum = \(\Pi\sb\texttt{i=k+1}\sp\texttt{n} \texttt{i}\)
;                    )
;              accum))
;          ;; Termination argument:
;          ;; \texttt{k} is a natural number that is decremented with each recursive call.
;          ;; Eventually, \texttt{k} becomes 0 and the recursion stops.
;          ]
;    (fact-state!)))


;; natnum --> natnum
;; Purpose: Compute the factorial of the given natural number
(define (fact2-v4 n)
  (local [;; natnum
          ;; Purpose: The next possible accum factor
          (define k (void))

          ;; natnum
          ;; Purpose: The current approximation of n!
          (define accum (void))]
    (begin
      (set! k n)
      (set! accum 1)
      ;; INV: accum = \(\Pi\sb\texttt{i=k+1}\sp\texttt{n} \texttt{i}\)
      (while (not (= k 0))
             ;; accum = \(\Pi\sb\texttt{i=k+1}\sp\texttt{n} \texttt{i}\) \logand k \(\neq\) 0
             (set! accum (* k accum))
             ;; accum = \(\Pi\sb\texttt{i=k}\sp\texttt{n} \texttt{i}\)
             (set! k (sub1 k))
             ;; accum = \(\Pi\sb\texttt{i=k+1}\sp\texttt{n} \texttt{i}\)
             )
      accum)
    ;; Termination argument:
    ;; \texttt{k} is a natural number that is decremented with each loop iteration.
    ;; Eventually, \texttt{k} becomes 0 and the loop stops.
    ))

;; Tests using sample values for fact2-v4
;(check-expect (fact2-v4 0)  1)
;(check-expect (fact2-v4 5)  120)
;(check-expect (fact2-v4 10) 3628800)
;(check-expect (fact2-v4 2)  2)
;(check-expect (fact2-v4 6)  720)
