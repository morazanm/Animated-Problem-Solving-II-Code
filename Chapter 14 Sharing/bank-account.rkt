;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname bank-account) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;; number >= 0
;; Purpose: Store the balance of the savings account
(define mybalance 'uninitialized)

;; number --> (void) throws error
;; Purpose: Initialize mybalance
;; Effect: mybalance is mutated to the given number
(define (initialize-mybalance! init-balance)
  (if (or (not (number? init-balance))
          (< init-balance 0))
      (error 'initialize-mybalance! "Balance cannot be initialized to the given value")
      (set! mybalance init-balance)))

;; Tests for initialize-mybalance!
(check-error (initialize-mybalance! "200")
             "initialize-mybalance!: Balance cannot be initialized to the given value")

(check-error (initialize-mybalance! -10)
             "initialize-mybalance!: Balance cannot be initialized to the given value")

(check-expect (begin
                (initialize-mybalance! 75)
                mybalance)
              75)
(check-within (begin
                (initialize-mybalance! 43.54)
                mybalance)
              43.54
              0.01)

;; number --> (void) throws error
;; Purpose: To make a deposit
;; Effect: The given amount is added to mybalance
(define (deposit! amt)
  (cond [(not (number? amt))
         (error 'deposit! "A deposit must be a positive number")]
        [(<= amt 0)
         (error 'deposit! "A deposit cannot be <= 0")]
        [else (set! mybalance (+ mybalance amt))]))

;; Tests for deposit!
(check-error (begin
               (initialize-mybalance! 100)
               (deposit! #t))
             "deposit!: A deposit must be a positive number")
(check-error (begin
               (initialize-mybalance! 100)
               (deposit! -100))
             "deposit!: A deposit cannot be <= 0")
(check-expect (begin
                (initialize-mybalance! 100)
                (deposit! 100)
                mybalance)
              200)
(check-expect (begin
                (initialize-mybalance! 5000)
                (deposit! 700)
                (deposit! 1300)
                mybalance)
              7000)


;; number --> (void) throws error
;; Purpose: To make a withdrawal
;; Effect: The given amount is subtracted from mybalance
(define (withdraw! amt)
  (cond [(or (not (number? amt)) (<= amt 0))
         (error 'withdraw! "The amount must be a positive number.")]
        [(< mybalance amt)
         (error 'withdraw! "Insufficient funds")]
        [else (set! mybalance (- mybalance amt))]))

;; Tests for withdraw!
(check-error (begin
               (initialize-mybalance! 22)
               (withdraw! (make-posn 0 0)))
             "withdraw!: The amount must be a positive number.")
(check-error (begin
               (initialize-mybalance! 50)
               (withdraw! -30))
             "withdraw!: The amount must be a positive number.")
(check-error (begin
               (initialize-mybalance! 160)
               (withdraw! 200))
             "withdraw!: Insufficient funds")
(check-error (begin
               (initialize-mybalance! 750)
               (withdraw! 900))
             "withdraw!: Insufficient funds")
(check-expect (begin
                (initialize-mybalance! 667)
                (withdraw! 500)
                mybalance)
              167)
(check-expect (begin
                (initialize-mybalance! 1500)
                (withdraw! 800)
                (withdraw! 200)
                mybalance)
              500)
             


;;  --> number
;; Purpose: Return the current balance
(define (get-balance) mybalance)

;; Tests for get-balance
(check-expect (begin
                (initialize-mybalance! 250)
                (get-balance))
              250)

(check-expect (begin
                (initialize-mybalance! 335)
                (deposit! 665)
                (withdraw! 200)
                (get-balance))
              800)

