;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname quiz-avg) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;; A quiz grade (quiz) is a number in [0..100]

;; Sample quiz grade
(define Q1 85)
(define Q2 100)

;; A list of quizzes (loq) is either:
;;  (list quiz)
;;  (cons quiz loq)

;; Sample loq
(define ALOQ1 (list Q1))
(define MYQZS (list Q2 90 Q2))

;; loq --> number
;; Purpose: Compute the quiz average
(define (quiz-avg a-loq)
  (/ (foldl (λ (q r) (+ q r)) 0 a-loq)
     (length a-loq)))

;; Sample Expressions for quiz-avg
(define ALOQ1-VAL (/ (foldl (λ (q r) (+ q r)) 0 ALOQ1)
                     (length ALOQ1)))

(define MYQZS-VAL (/ (foldl (λ (q r) (+ q r)) 0 MYQZS)
                     (length MYQZS)))

;; Tests using sample computations for quiz-avg
(check-within (quiz-avg ALOQ1) ALOQ1-VAL 0.01)
(check-within (quiz-avg MYQZS) MYQZS-VAL 0.01)

;; Tests using sample values for quiz-avg
(check-within (quiz-avg '(80 80 70 75 70)) 75 0.01)

;; quiz --> (void)
;; Purpose: Add the given quiz grade to MYQZS
;; Effect: Add the given quiz grade to the front of MYQZS
(define (add-quiz! q)
  (set! MYQZS (cons q MYQZS)))

;; Effect tests for add-quiz
(check-expect (begin
                (add-quiz! 90)
                (add-quiz! 92)
                MYQZS)
              (list 92 90 100 90 100))
(check-within (quiz-avg MYQZS) 94.4 0.01)
(check-expect (begin
                (add-quiz! 91)
                MYQZS)
              (list 91 92 90 100 90 100))

