;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname max-lon) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define (max-lon a-lon)
  (if (empty? (rest a-lon))
      (first a-lon)
      (local [(define max-of-rest (max-lon (rest a-lon)))]
        (if (> max-of-rest (first a-lon))
            max-of-rest
            (first a-lon)))))

(check-expect (max-lon (list 1 2 3)) 3)
(check-expect (max-lon (list -8 -13 -13 0 -44)) 0)

(define (max-lon-accum a-lon max-so-far)
  (if (empty? a-lon)
      max-so-far
      (max-lon-accum (rest a-lon)
                     (if (> (first a-lon) max-so-far)
                         (first a-lon)
                         max-so-far))))

(define (max-lon2 a-lon)
  (max-lon-accum (rest a-lon) (first a-lon)))

(check-expect (max-lon2 (list 1 2 3)) 3)
(check-expect (max-lon2 (list -8 -13 -13 0 -44)) 0)

(define L (build-list 1000000 (lambda (i) (random 1000000))))

(define M1 (time (max-lon L)))
(define M2 (time (max-lon2 L)))


(define (max-lon3 a-lon)
  (foldl (λ (max-so-far a-natnum)
           (if (> a-natnum max-so-far)
               a-natnum
               max-so-far))
         -1
         a-lon))

(check-expect (max-lon3 (list 1 2 3)) 3)
(check-expect (max-lon3 (list -8 -13 -13 0 -44)) 0)
               
(define M3 (time (max-lon3 L)))

