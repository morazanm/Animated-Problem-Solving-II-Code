;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname length-lostr) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define ELOSTR '())
(define LOSTR1 '("a" "b" "c"))
(define LOSTR2 '("Program" "design" "is" "awesome!"))

;; (listof string) --> (listof natnum)
;; Purpose: Return the lengths of the strings in the given
;;          list of strings
(define (lengths-lostr a-lostr)
  (if (empty? a-lostr)
      '()
      (cons (string-length (first a-lostr))
            (lengths-lostr (rest a-lostr)))))

;; Sample expressions for lengths-lostr
(define LENS-ELOSTR '())
(define LENS-LOSTR1 (cons (string-length (first LOSTR1))
                          (lengths-lostr (rest  LOSTR1))))
(define LENS-LOSTR2 (cons (string-length (first LOSTR2))
                          (lengths-lostr (rest  LOSTR2))))

;; Tests using sample computations for lengths-lostr
(check-expect (lengths-lostr ELOSTR) LENS-ELOSTR)
(check-expect (lengths-lostr LOSTR1) LENS-LOSTR1)
(check-expect (lengths-lostr LOSTR2) LENS-LOSTR2)

;; Tests using sample values for lengths-lostr
(check-expect (lengths-lostr '("I" "love" "invariants."))
              '(1 4 11))
(check-expect (lengths-lostr '("Accumulators" "Rock!"))
              '(12 5))


;; (listof string) (listof natnum) --> (listof natnum)
;; Purpose: Return the lengths of the strings in the given
;;          list of strings in reversed order
;; Accumulator invariant:
;;   accum = traversed list string lengths in reversed order
(define (lengths-lostr-accum a-lostr accum)
  (if (empty? a-lostr)
      accum
      (lengths-lostr-accum
       (rest a-lostr)
       (cons (string-length (first a-lostr)) accum))))

;; Sample expressions for lengths-lostr-accum
(define LACC2-ELOSTR1 '())
(define LACC2-ELOSTR2 '("1" "1" "1"))
(define LACC2-LOSTR1 (lengths-lostr-accum
                      (rest LOSTR1)
                      (cons (string-length (first LOSTR1)) '())))
(define LACC2-LOSTR2 (lengths-lostr-accum
                      (rest (rest LOSTR2))
                      (cons (string-length (second LOSTR2))
                            (list (first LOSTR2)))))

;; Tests using sample computations for lengths-lostr-accum
(check-expect (lengths-lostr-accum ELOSTR '())
              LACC2-ELOSTR1)
(check-expect (lengths-lostr-accum
               ELOSTR
               '("1" "1" "1"))
              LACC2-ELOSTR2)
(check-expect (lengths-lostr-accum LOSTR1 '())
              LACC2-LOSTR1)
(check-expect (lengths-lostr-accum
               (rest LOSTR2)
               (list (first LOSTR2)))
              LACC2-LOSTR2)

;; Tests using sample values for lengths-lostr-accum
(check-expect (lengths-lostr-accum
               '("invariants." "love" "I")
               '())
              '(1 4 11))
(check-expect (lengths-lostr-accum
               '("Rock!" "Accumulators")
               '())
              '(12 5))

;; (listof string) --> (listof natnum)
;; Purpose: Return the lengths of the strings in the given
;;          list of strings
(define (lengths-lostr2 a-lostr)
  (lengths-lostr-accum (reverse a-lostr) '()))

;; Sample expressions for lengths-lostr2
(define LENS2-ELOSTR (lengths-lostr-accum
                      (reverse ELOSTR) '()))
(define LENS2-LOSTR1 (lengths-lostr-accum
                      (reverse LOSTR1) '()))
(define LENS2-LOSTR2 (lengths-lostr-accum
                      (reverse LOSTR2) '()))

;; Tests using sample computations for lengths-lostr2
(check-expect (lengths-lostr2 ELOSTR) LENS2-ELOSTR)
(check-expect (lengths-lostr2 LOSTR1) LENS2-LOSTR1)
(check-expect (lengths-lostr2 LOSTR2) LENS2-LOSTR2)

;; Tests using sample values for lengths-lostr2
(check-expect (lengths-lostr '("I" "love" "invariants."))
              '(1 4 11))
(check-expect (lengths-lostr '("Accumulators" "Rock!"))
              '(12 5))

;; (listof string) --> (listof natnum)
;; Purpose: Return the lengths of the strings in the given
;;          list of strings
(define (lengths-lostr5 a-lostr)
  (foldr (λ (a-str acc)
           (cons (string-length a-str) acc))
         '()
         a-lostr))

;; Sample expressions for lengths-lostr5
(define LENS5-ELOSTR (foldr (λ (a-str acc)
                             (cons (string-length a-str) acc))
                           '()
                           ELOSTR))
(define LENS5-LOSTR1 (foldr (λ (a-str acc)
                             (cons (string-length a-str)
                                   acc))
                           '()
                           LOSTR1))
(define LENS5-LOSTR2 (foldr (λ (a-str acc)
                             (cons (string-length a-str) acc))
                           '()
                           LOSTR2))

;; Tests using sample computations for lengths-lostr5
(check-expect (lengths-lostr5 ELOSTR) LENS5-ELOSTR)
(check-expect (lengths-lostr5 LOSTR1) LENS5-LOSTR1)
(check-expect (lengths-lostr5 LOSTR2) LENS5-LOSTR2)

;; Tests using sample values for lengths-lostr5
(check-expect (lengths-lostr5 '("I" "love" "invariants."))
              '(1 4 11))
(check-expect (lengths-lostr5 '("Accumulators" "Rock!"))
              '(12 5))

(define UCASE (map symbol->string
                   '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))
(define LCASE (map string-downcase UCASE))
(define NUMBS (map number->string '(0 1 2 3 4 5 6 7 8 9)))
(define SCHRS '("$" "&" "*" "+" "@" "#" "_" "!" "?"))

          

;; natnum --> pw throws error
;; Purpose: To generate a password of the given length
(define (generate-password passwd-len)
  (if (< passwd-len 10)
      (error "The password length must be at least 10.")
      (local [(define UC-NUM (add1 (random 7))) 
              (define LC-NUM (add1 (random (- passwd-len UC-NUM 2))))
              (define NB-NUM (add1 (random (- passwd-len UC-NUM LC-NUM 1))))
              (define SC-NUM (- passwd-len UC-NUM LC-NUM NB-NUM))

              ;; (listof X) natnum --> (listof X)
              ;; Purpose: Randomly pick the given number of elements from the given list
              (define (pick lst n)
                (build-list n (λ (i) (list-ref lst (random (length lst))))))

              ;; (listof string) (listof string) (listof string) (listof string) --> string
              ;; Purpose: Generate a string using all the strings in the given lists
              (define (generate L0 L1 L2 L3)
                (if (and (empty? L0) (empty? L1) (empty? L2) (empty? L3))
                    ""
                    (local [(define lst-num (random 4))]
                      (cond
                        [(= lst-num 0) (if (empty? L0)
                                           (generate L0 L1 L2 L3)
                                           (string-append (first L0) (generate (rest L0) L1 L2 L3)))]
                        [(= lst-num 1) (if (empty? L1)
                                           (generate L0 L1 L2 L3)
                                           (string-append (first L1) (generate L0 (rest L1) L2 L3)))]
                        [(= lst-num 2) (if (empty? L2)
                                           (generate L0 L1 L2 L3)
                                           (string-append (first L2) (generate L0 L1 (rest L2) L3)))]
                        [(= lst-num 3) (if (empty? L3)
                                           (generate L0 L1 L2 L3)
                                           (string-append (first L3) (generate L0 L1 L2 (rest L3))))]))))]
        (generate (pick UCASE UC-NUM)
                  (pick LCASE LC-NUM)
                  (pick NUMBS NB-NUM)
                  (pick SCHRS SC-NUM)))))


(define L (build-list 100000 (λ (i) (generate-password (+ 10 (random 20))))))

(define T01 (time (lengths-lostr  L)))
(define T02 (time (lengths-lostr2 L)))
(define T03 (time (lengths-lostr5 L)))

(define T11 (time (lengths-lostr  L)))
(define T12 (time (lengths-lostr2 L)))
(define T13 (time (lengths-lostr5 L)))

(define T21 (time (lengths-lostr  L)))
(define T22 (time (lengths-lostr2 L)))
(define T23 (time (lengths-lostr5 L)))

(define T31 (time (lengths-lostr  L)))
(define T32 (time (lengths-lostr2 L)))
(define T33 (time (lengths-lostr5 L)))

(define T41 (time (lengths-lostr  L)))
(define T42 (time (lengths-lostr2 L)))
(define T43 (time (lengths-lostr5 L)))
