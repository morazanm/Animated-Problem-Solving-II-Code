;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname bank-account-interface) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

#|
  A bank account, ba, is an interface that offers the following services:
    1.        'init: number --> (void)
    2. 'get-balance: number
    3.     'deposit: number --> (void)
    4.  'withdrawal: number --> (void)

TEMPLATE FOR ba CLASS
;;  --> ba
;; Purpose: To contruct an bank account
(define (make-bankaccount)
  (local [;; number >= 0
          ;; Purpose: Store the balance of the bank account
          (define mybalance 'uninitialized)

          ;; number --> (void) throws error
          ;; Purpose: Initialize mybalance
          ;; Effect: mybalance is mutated to the given number
          (define (initialize-mybalance! init-balance) ...)

          ;; number --> (void) throws error
          ;; Purpose: To make a deposit
          ;; Effect: The given amount is added to mybalance
          (define (deposit! amt) ...)

          ;; number --> (void) throws error
          ;; Purpose: To make a withdrawal
          ;; Effect: The given amount is subtracted from mybalance
          (define (withdraw! amt) ...)

          ;;  --> number
          ;; Purpose: Return the current balance
          (define (get-balance) ...)

          ;; message --> ba 
          ;; Purpose: To manage bank account services
          (define (bank-account-object a-message)
            (cond [(eq? a-message 'init) initialize-mybalance!]
                  [(eq? a-message 'get-balance) mybalance]
                  [(eq? a-message 'deposit) deposit!]
                  [(eq? a-message 'withdraw) withdraw!]
                  [else (error 'bank-account-object (format "Unknown message received: ~s" a-message))]))]
      bank-account-object))
|#
;;  --> ba
;; Purpose: To contruct an bank account
(define (make-bankaccount)
  (local [;; number >= 0
          ;; Purpose: Store the balance of the bank account
          (define mybalance 'uninitialized)

          ;; number --> (void) throws error
          ;; Purpose: Initialize mybalance
          ;; Effect: mybalance is mutated to the given number
          (define (initialize-mybalance! init-balance)
            (if (or (not (number? init-balance))
                    (< init-balance 0))
                (error 'initialize-mybalance! "Balance cannot be initialized to the given value")
                (set! mybalance init-balance)))

          ;; number --> (void) throws error
          ;; Purpose: To make a deposit
          ;; Effect: The given amount is added to mybalance
          (define (deposit! amt)
            (cond [(not (number? amt))
                   (error 'deposit! "A deposit must be a positive number")]
                  [(<= amt 0)
                   (error 'deposit! "A deposit cannot be <= 0")]
                  [else (set! mybalance (+ mybalance amt))]))

          ;; number --> (void) throws error
          ;; Purpose: To make a withdrawal
          ;; Effect: The given amount is subtracted from mybalance
          (define (withdraw! amt)
            (cond [(or (not (number? amt)) (<= amt 0))
                   (error 'withdraw! "The amount must be a positive number.")]
                  [(< mybalance amt)
                   (error 'withdraw! "Insufficient funds")]
                  [else (set! mybalance (- mybalance amt))]))

          ;;  --> number
          ;; Purpose: Return the current balance
          (define (get-balance) mybalance)

          ;; message --> bank account service
          ;; Purpose: To manage bank account services
          (define (bank-account-object a-message)
            (cond [(eq? a-message 'get-balance) mybalance]
                  [(eq? a-message 'init) initialize-mybalance!]
                  [(eq? a-message 'deposit) deposit!]
                  [(eq? a-message 'withdraw) withdraw!]
                  [else (error 'bank-account-object (format "Unknown message received: ~s" a-message))]))]
    bank-account-object))

;; Sample ba
(define acct1 (make-bankaccount))
(define acct2 (make-bankaccount))


;; ba --> number
;; Purpose: To get the balance of the given bank account
(define (get-balance ba) (ba 'get-balance))

;; Tests using sample computations for get-balance
(check-expect (begin
                (initialize! acct1 500)
                (get-balance acct1))
              500)

(check-expect (begin
                (initialize! acct2 200)
                (get-balance acct2))
              200)


;; ba number --> (void) throws error
;; Purpose: To initialize the given bank account with the given balance
;; Effect: Change the given ba's balance to the given number
(define (initialize! ba amt) ((ba 'init) amt))

;; Tests using sample computations for initialize
(check-error (initialize! acct1 -78)
             "initialize-mybalance!: Balance cannot be initialized to the given value")

(check-error (initialize! acct1 #t)
             "initialize-mybalance!: Balance cannot be initialized to the given value")

(check-expect (begin
                (initialize! acct1 3400)
                (get-balance acct1))
              3400)

(check-expect (begin
                (initialize! acct2 788)
                (get-balance acct2))
              788)


;; ba number --> (void) throws error
;; Purpose: To deposit the given amount in the given bank account
;; Effect: Increase the balance of the given ba by the given amount
(define (make-deposit! ba amt) ((ba 'deposit) amt))

;; Tests using sample computations for make-deposit
(check-error (begin
               (initialize! acct1 500)
               (make-deposit! acct1 'a))
             "deposit!: A deposit must be a positive number")

(check-error (begin
               (initialize! acct1 500)
               (make-deposit! acct1 -300))
             "deposit!: A deposit cannot be <= 0")

(check-expect (begin
                (initialize! acct1 500)
                (make-deposit! acct1 300)
                (get-balance acct1))
              800)

(check-expect (begin
                (initialize! acct2 200)
                (make-deposit! acct2 550)
                (make-deposit! acct2 250)
                (get-balance acct2))
              1000)

               

;; bankaccount number --> (void)
;; Purpose: To withdraw the given amount in the given bank account
;; Effect: Decrease the balance of the given ba by the given amount
(define (make-withdrawal! ba amt) ((ba 'withdraw) amt))

;; Tests using sample computations for make-withdrawal
(check-error (begin
               (initialize! acct1 10)
               (make-withdrawal! acct1 'a))
             "withdraw!: The amount must be a positive number.")

(check-error (begin
               (initialize! acct1 45)
               (make-withdrawal! acct1 -10))
             "withdraw!: The amount must be a positive number.")

(check-error (begin
               (initialize! acct1 750)
               (make-withdrawal! acct1 800))
             "withdraw!: Insufficient funds")

(check-expect (begin
                (initialize! acct1 500)
                (make-withdrawal! acct1 250)
                (get-balance acct1))
              250)

(check-expect (begin
                (initialize! acct2 200)
                (make-withdrawal! acct2 200)
                (make-deposit! acct2 400)
                (get-balance acct2))
              400)


