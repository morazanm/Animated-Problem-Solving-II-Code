;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname append-strings) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

(define LSTRE '())
(define LSTR1 '("2" "3" "89" "-1" "-3" "2"))
(define LSTR2 '("a" "b" "c" "d"))

;; (listof string) --> string
;; Purpose: Append strings in the given the list of strings
;;          into a single string
(define (append-strings a-lostr)
  (if (empty? a-lostr)
      ""
      (string-append (first a-lostr) (append-strings (rest a-lostr)))))

;; Sample expressions for append-strings
(define LSTRE-VAL "")
(define LSTR1-VAL (string-append (first LSTR1)
                                 (append-strings (rest LSTR1))))
(define LSTR2-VAL (string-append (first LSTR2)
                                 (append-strings (rest LSTR2))))

;; Tests using sample computations for append-strings
(check-expect (append-strings LSTRE)  LSTRE-VAL)
(check-expect (append-strings LSTR1)  LSTR1-VAL)
(check-expect (append-strings LSTR2) LSTR2-VAL)

;; Tests using sample values for append-strings
(check-expect (append-strings '("I" " LOVE " "INVARIANTS!"))
              "I LOVE INVARIANTS!")
(check-expect (append-strings '("CS " "ROCKS"))
              "CS ROCKS")


;; (listof string) string --> string
;; Purpose: Append strings in the given the list of strings
;;          into a single string in reversed order
;; Accumulator invariant:
;;  accum = the appended strings so far in reversed order
(define (append-strings-accum a-lostr accum)
  (if (empty? a-lostr)
      accum
      (append-strings-accum (rest a-lostr)
                            (string-append (first a-lostr)
                                           accum))))

;; Sample expressions for append-strings-accum
(define LSTRE-ACCV1 "")
(define LSTRE-ACCV2 "2389-1-12")
(define LSTR1-ACCV (append-strings-accum
                     (rest LSTR1)
                     (string-append (first LSTR1)
                                    "")))
(define LSTR2-ACCV (append-strings-accum
                     (rest (rest LSTR2))
                     (string-append (first (rest LSTR2))
                                    (first LSTR2))))

;; Tests using sample computations for append-strings-accum
(check-expect (append-strings-accum LSTRE "") LSTRE-ACCV1)
(check-expect (append-strings-accum LSTRE "2389-1-12") LSTRE-ACCV2)
(check-expect (append-strings-accum LSTR1 "") LSTR1-ACCV)
(check-expect (append-strings-accum (rest LSTR2) (first LSTR2))
              LSTR2-ACCV)

;; Tests using sample values for append-strings-accum
(check-expect (append-strings-accum '("c" "b" "a") "") "abc")
(check-expect (append-strings-accum '("CS") "") "CS")
  

;; (listof string) --> string
;; Purpose: Append strings in the given the list of strings
;;          into a single string
(define (append-strings2 a-lostr)
  (append-strings-accum (reverse a-lostr) ""))

;; Sample expressions for append-strings2
(define LSTRE-VAL2 (append-strings-accum (reverse LSTRE) ""))
(define LSTR1-VAL2 (append-strings-accum (reverse LSTR1) ""))
(define LSTR2-VAL2 (append-strings-accum (reverse LSTR2) ""))

;; Tests using sample computations for append-strings2
(check-expect (append-strings2 LSTRE)  LSTRE-VAL5)
(check-expect (append-strings2 LSTR1)  LSTR1-VAL5)
(check-expect (append-strings2 LSTR2) LSTR2-VAL5)

;; Tests using sample values for append-strings2
(check-expect (append-strings2 '("I" " LOVE " "INVARIANTS!"))
              "I LOVE INVARIANTS!")
(check-expect (append-strings2 '("CS " "ROCKS"))
              "CS ROCKS")


;; (listof string) --> string
;; Purpose: Append strings in the given the list of strings
;;          into a single string
(define (append-strings5 a-lostr)
  (foldr (λ (str acc) (string-append str acc)) "" a-lostr))

;; Sample expressions for append-strings
(define LSTRE-VAL5 (foldr (λ (str acc)
                            (string-append str acc))
                          ""
                          LSTRE))
(define LSTR1-VAL5 (foldr (λ (str acc)
                            (string-append str acc))
                          ""
                          LSTR1))
(define LSTR2-VAL5 (foldr (λ (str acc)
                            (string-append str acc))
                          ""
                          LSTR2))

;; Tests using sample computations for append-strings5
(check-expect (append-strings5 LSTRE)  LSTRE-VAL5)
(check-expect (append-strings5 LSTR1)  LSTR1-VAL5)
(check-expect (append-strings5 LSTR2) LSTR2-VAL5)

;; Tests using sample values for append-strings5
(check-expect (append-strings5 '("I" " LOVE " "INVARIANTS!"))
              "I LOVE INVARIANTS!")
(check-expect (append-strings5 '("CS " "ROCKS"))
              "CS ROCKS")


