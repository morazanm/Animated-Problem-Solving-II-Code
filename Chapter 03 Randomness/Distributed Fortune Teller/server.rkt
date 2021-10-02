;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname server) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

#|
     A to-server message, tsm, is either:
       1. \textquotesingle{bye}
       2. \textquotesingle{send-fortune}
|#

;; Sample tsm
(define BYE 'bye)
(define SFT 'send-fortune)

#|
     ;; tsm ... --> ...
     ;; Purpose:
     (define (f-on-tsm a-tsm ...)
       (if (eq? a-tsm 'bye)
           ...
           ...))

     ;; Sample expressions for f-on-tsm
     (define BYE-VAL ...)
     (define SFT-VAL ...)

     ;; Tests using sample computations for f-on-tsm
     (check-expect (f-on-tsm BYE ...) BYE-VAL)
     (check-expect (f-on-tsm SFT ...) SFT-VAL)

     ;; Tests using sample values for f-on-tsm
     (check-expect (f-on-tsm ... ...) ...)
          ...
|#


(define FORTUNES '("Do not violate the prime directive"
                   "0.67 seconds is an eternity for an android"
                   "Don't be left with nothing but your bones"
                   "Stay down, honor has been served"
                   "Reach for the final frontier"
                   "I canna' change the laws of physics"
                   "No one can summon the future"
                   "You can use logic to justify almost anything"
                   "There is a way out of every box"
                   "Make it so"
                   "Don't grieve if it is logical"
                   "Live long and prosper"
                   "Resistance is futile"
                   "Set your phasers to stun"
                   "Without followers, evil cannot spread"
                   "I fail to comprehend your indignation"
                   "Things are only impossible until they’re not"
                   "A lie is a very poor way to say hello"
                   "Time is the fire in which we burn"
                   "Make now always the most precious time"
                   "Perhaps man wasn’t meant for paradise"
                   "To survive is not enough"
                   "To be human is to be complex"))

;; A universe is a (listof iworld)

(define INIT-UNIV '())
(define UNIV2 (list iworld1))

;; universe iworld --> bundle
;; Purpose: Add the given world to the given universe
(define (add-new-iworld u iw)
  (if (member? (iworld-name iw) (map iworld-name u))
      (make-bundle u
                   (list (make-mail iw 'connection-denied))
                   '())
      (make-bundle (cons iw u) '() '())))

;; Sample expressions for add-new-world
(define ADD1 (make-bundle (cons iworld1 INIT-UNIV) '() '()))
(define ADD2 (make-bundle (cons iworld2 UNIV2) '() '()))
(define ADD3 (make-bundle UNIV2 (list (make-mail iworld1 'connection-denied)) '()))

;; Tests using sample computation for add-new-world
(check-expect (add-new-iworld INIT-UNIV iworld1) ADD1)
(check-expect (add-new-iworld UNIV2     iworld2) ADD2)
(check-expect (add-new-iworld UNIV2     iworld1) ADD3)

;; Tests using sample values for add-new-world
(check-expect (add-new-iworld (list iworld2) iworld3)
              (make-bundle (list iworld3 iworld2) '() '()))
(check-expect (add-new-iworld (list iworld2 iworld3) iworld3)
              (make-bundle (list iworld2 iworld3)
                           (list (make-mail iworld3 'connection-denied))
                           '()))


;; universe iworld --> universe
;; Purpose: Remove the given iworld from the given universe
(define (rm-iworld u iw)
  (filter (λ (w) (not (equal? w iw)))  u))

;; Sample expressions for rm-iworld
(define RM1 (filter (λ (w) (not (equal? w iworld2)))  UNIV2))
(define RM2 (filter (λ (w) (not (equal? w iworld1)))  INIT-UNIV))

;; Tests using sample computations for rm-iworld
(check-expect (rm-iworld UNIV2 iworld2) RM1)
(check-expect (rm-iworld UNIV2 iworld1) RM2)

;; Tests using sample values for rm-iworld
(check-expect (rm-iworld (list iworld1 iworld2 iworld3)
                         iworld2)
              (list iworld1 iworld3))




;; universe iworld tsm --> bundle
;; Purpose: To process the given to-server message
(define (process-message u iw m)
  (if (eq? m 'bye)
      (make-bundle (rm-iworld u iw)
                   '()
                   (list iw))
      (make-bundle u
                   (list (make-mail iw (list-ref FORTUNES (random (length FORTUNES)))))
                   '())))

;; Sample expressions for process-message
(define PM1 (make-bundle (filter (λ (w)
                                   (not (equal? (iworld-name w)
                                                (iworld-name iworld1))))
                                 UNIV2)
                         '()
                         (list iworld1)))

(define PM2 (make-bundle UNIV2
                         (list (make-mail iworld1 (list-ref FORTUNES (random (length FORTUNES)))))
                         '()))

;; can't predict which fortune is chosen for a 'send-fortune tsm
;; (check-expect (process-message UNIV2 iworld1 'send-fortune) PM2)
;; will not always pass

;; Tests using sample computations for process-message
(check-expect (process-message UNIV2 iworld1 'bye) PM1)
(check-random (process-message UNIV2 iworld1 'send-fortune)
              (make-bundle UNIV2
                           (list (make-mail iworld1 (list-ref FORTUNES (random (length FORTUNES)))))
                           '()))

;; Tests using sample values for process-message
(check-random (process-message (list iworld3 iworld2) iworld2 'send-fortune)
              (make-bundle (list iworld3 iworld2)
                           (list (make-mail iworld2 (list-ref FORTUNES (random (length FORTUNES)))))
                           '()))
(check-random (process-message (list iworld1 iworld2) iworld1 'send-fortune)
              (make-bundle (list iworld1 iworld2)
                           (list (make-mail iworld1 (list-ref FORTUNES (random (length FORTUNES)))))
                           '()))



;; Any --> universe
;; Purpose: Run the universe server
(define (run-server a-z)
  (universe
   INIT-UNIV
   (on-new add-new-iworld)
   (on-msg process-message)
   (on-disconnect rm-iworld)))