;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname max-lon) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require while)

(define L0 '())
(define L1 '(1 2 3 4 5 6))
(define L2 '(5 4 3 2 1 0 -1))
(define L3 '(9 0 -22 54 10 -89))

;; lon --> number throws error
;; Purpose: Return the maximum of the given list
(define (max-lon L)
  (if (empty? L)
      (error "The empty list does not have a maximum element.")
  (local [;; lon
          ;; Purpose: Unprocessed part of L
          (define ul (void))

          ;; number
          ;; Purpose: The maximum in the processed part of L
          (define max (void))]
    (begin
      (set! ul L)
      (set! max -inf.0)
      ;; INV: L = (append (L - ul) ul) AND max = maximum(L-ul)
      (while (not (empty? ul))
             ;; L = (append (L - ul) ul) AND max = maximum(L-ul)
             (if (> (first ul) max)
                 (begin
                 ;; L = (append (L - ul) ul) AND max = maximum(L-ul) AND (> (first ul) max)
                 (set! max (first ul))
                 ;; L = (append (L - ul) ul) AND max = maximum(L-(rest ul))
                 (set! ul (rest ul))
                 ;; L = (append (L - ul) ul) AND max = maximum(L-ul)
                 )
                 ;; else
                 ;; L = (append (L - ul) ul) AND max = maximum(L-ul) AND (<= (first ul) max)
                 (set! ul (rest ul))
                 ;; L = (append (L - ul) ul) AND max = maximum(L-ul)
                 ))
      ;; L = (append (L - ul) ul) AND max = maximum(L-ul) AND (empty? ul)
      ;; ==> max = maximum(L)
      max))))

;; Tests for max-lon
(check-error  (max-lon L0) "The empty list does not have a maximum element.")
(check-expect (max-lon L1) 6)
(check-expect (max-lon L2) 5)
(check-expect (max-lon L3) 54)
             