;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname generate-password) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

#|
A password, pw, is a string that has a minimum length of 10 and
contains at least one of each: lower case letter, upper case
letter, number, and special character.
|#

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

;; Tests using sample values for generate-password
(check-error     (generate-password 6) "The password length must be at least 10.")
(check-satisfied (generate-password 10) valid-passwd?)
(check-satisfied (generate-password 12) valid-passwd?)

;; string natnum --> Boolean
;; Purpose: Determine of the given string is a pw
(define (valid-passwd? str)
  (local [(define lststr (explode str))
          (define uc (filter (λ (s) (member s UCASE)) lststr))
          (define lc (filter (λ (s) (member s LCASE)) lststr))
          (define nc (filter (λ (s) (member s NUMBS)) lststr))
          (define sc (filter (λ (s) (member s SCHRS)) lststr))]
    (and (>= (string-length str) 10)
         (= (length lststr) (+ (length uc) (length lc) (length nc) (length sc)))
         (> (length uc) 0)
         (> (length lc) 0)
         (> (length nc) 0)
         (> (length sc) 0))))

;; Tests using sample values for valid-passwd?
(check-expect (valid-passwd? "?$Isdwer67G")   #true)
(check-expect (valid-passwd? "Z!jwqx8t2sY32") #true)
(check-expect (valid-passwd? "abcdefgh")      #false)
(check-expect (valid-passwd? "a2cZdEfghJ8")   #false)
;(check-expect (valid-passwd? "a2cZd/fghJ8")   #false)





