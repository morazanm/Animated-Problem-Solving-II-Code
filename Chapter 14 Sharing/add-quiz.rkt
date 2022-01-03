;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname add-quiz) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;; A quiz grade (quiz) is a number in [0..100]

;; Sample quiz grade
(define Q1 85)
(define Q2 100)

;; A list of quizzes (loq) is either:
;;  (list quiz)
;;  (cons quiz loq)


;; Purpose: A loq for this semester 
(define MYQZS 'uninitialized)
     
;;  --> (void)
;; Purpose: Initialize MYQZS
;; Effect:  MYQZS is initialized to \textquotesingle{}(100 90 100)
(define (initialize-myqzs) 
  (set! MYQZS '(100 90 100)))
     
;; quiz --> (void)
;; Purpose: Add the given quiz grade to MYQZS
;; Effect: Add the given quiz grade to the front of MYQZS
(define (add-quiz! q)
  (set! MYQZS (cons q MYQZS)))

(check-expect (begin
                (initialize-myqzs)
                (add-quiz! 90)
                (add-quiz! 92)
                MYQZS)
              (list 92 90 100 90 100))

(check-expect (begin
                (initialize-myqzs)
                (add-quiz! 88)
                MYQZS)
              (list 88 100 90 100))

#| This test fails: Loss of referential transparency.
(check-expect (begin
                (add-quiz! 88)
                MYQZS)
              (begin
                (add-quiz! 88)
                MYQZS))
|#

