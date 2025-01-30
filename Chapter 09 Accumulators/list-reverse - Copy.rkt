;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname list-reverse) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; (listof X) --> (listof X)
;; Purpose: To reverse the given list
(define (rev L)
  (if (empty? L)
      '()
      (append (rev (rest L)) (list (first L)))))

;; Tests
(check-expect (rev '()) '())
(check-expect (rev '(1 2 3 4 5)) '(5 4 3 2 1))
(check-random (rev (build-list 1500 (λ (i) (random 100000))))
              (reverse (build-list 1500 (λ (i) (random 100000)))))


;; (listof X) --> (listof X)
;; Purpose: To reverse the given list
(define (rev2 L)
  (local [;; (listof X) (listof X) --> (listof X)
          ;; Purpose: Reverse the first given list
          ;; Accumulator invariant: acc = the reverse of the previously processed list elements
          (define (rev2-helper L acc)
            (if (empty? L)
                acc
                (rev2-helper (rest L) (cons (first L) acc))))]
    (rev2-helper L '())))

;; Tests
(check-expect (rev2 '()) '())
(check-expect (rev2 '(1 2 3 4 5)) '(5 4 3 2 1))
(check-random (rev2 (build-list 1500 (λ (i) (random 100000))))
              (reverse (build-list 1500 (λ (i) (random 100000)))))