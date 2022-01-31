;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname merge-sorted-vectors) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require while)

;; Sample (vectorof number)
(define V0 (vector))
(define V1 (vector 1 2 3))
(define V2 (vector -8 -4 -2 10))
(define V3 (vector 4 5 6))

;; (vectorof number) (vectorof number) --> (vectorof number)
;; Purpose: To merge the two given vectors
;; Assumption: The given vectors are sorted in nondecreasing order
(define (merge-vectors V1 V2)
  (local [;; (vectorof number)
          ;; Purpose: Stores V1 and V2 elements in nondecreasing order
          (define res (build-vector (+ (vector-length V1)
                                       (vector-length V2))
                                    (λ (i) (void))))

          ;; int
          ;; Purpose: Store lowest unprocessed index for V1
          (define lowV1 (void))

          ;; int
          ;; Purpose: Store lowest unprocessed index for V2
          (define lowV2 (void))

          ;; int
          ;; Purpose: Next res index to store into
          (define lowres (void))]
    (begin
      (set! lowV1 0)
      (set! lowV2 0)
      (set! lowres 0)
      #| INV:  res[0..lowres-1] = (V1[0..lowV1-1] U V2[0..lowV2-1])
                                  in nondecreasing order
       AND 0 <= lowres <= (vector-length res)
       AND 0 <= lowV1 <= (vector-length V1)
       AND 0 <= lowV2 <= (vector-length V2)
       AND lowres = lowV1 + lowV2
      |#
      (while (<= lowres (sub1 (vector-length res)))
             #|   res[0..lowres-1] = (V1[0..lowV1-1] U V2[0..lowV2-1])
                                     in nondecreasing order
               AND 0 <= lowres < (vector-length res)
               AND 0 <= lowV1 <= (vector-length V1)
               AND 0 <= lowV2 <= (vector-length V2)
               AND lowres = lowV1 + lowV2
               AND lowres < (sub1 (vector-length res))
               ==>    lowV1 < (vector-length V1)
                   OR lowV2 < (vector-length V2)
             |#
             (cond [(and (< lowV1 (vector-length V1))
                         (< lowV2 (vector-length V2)))
                    #|   res[0..lowres-1] = (V1[0..lowV1-1] U V2[0..lowV2-1])
                          in nondecreasing order
                      AND 0 <= lowres < (vector-length res)
                      AND 0 <= lowV1 < (vector-length V1)
                      AND 0 <= lowV2 < (vector-length V2)
                      AND lowres = lowV1 + lowV2
                    |#
                    (if (<= (vector-ref V1 lowV1) (vector-ref V2 lowV2))
                        (begin
                          #|   res[0..lowres-1] = (V1[0..lowV1-1] U V2[0..lowV2-1])
                                                  in nondecreasing order
                            AND 0 <= lowres < (vector-length res)
                            AND 0 <= lowV1 < (vector-length V1)
                            AND 0 <= lowV2 <= (vector-length V2)
                            AND lowres = lowV1 + lowV2
                            AND V1[lowV1]  <=  V2[lowV2]
                          |#
                          (vector-set! res lowres (vector-ref V1 lowV1))
                          #|   res[0..lowres] = (V1[0..lowV1] U V2[0..lowV2-1])
                                                in nondecreasing order
                            AND 0 <= lowres < (vector-length res)
                            AND 0 <= lowV1 < (vector-length V1)
                            AND 0 <= lowV2 <= (vector-length V2)
                            AND lowres = lowV1 + lowV2
                          |#
                          (set! lowV1 (add1 lowV1))
                          #|   res[0..lowres] = (V1[0..lowV1-1] U V2[0..lowV2-1])
                                                in nondecreasing order
                            AND 0 <= lowres < (vector-length res)
                            AND 0 <= lowV1 <= (vector-length V1)
                            AND 0 <= lowV2 <= (vector-length V2)
                            AND lowres = lowV1-1 + lowV2
                          |#
                          (set! lowres (add1 lowres))
                          ;; INV
                          )
                        (begin
                          #|   res[0..lowres-1] = (V1[0..lowV1-1] U (V2[0..lowV2-1])
                                                  in nondecreasing order
                            AND 0 <= lowres<(vector-length res)
                            AND 0 <= lowV1 <= (vector-length V1)
                            AND 0 <= lowV2<(vector-length V2)
                            AND lowres = lowV1 + lowV2
                            AND V1[lowV1] > V2[lowV2]
                          |#
                          (vector-set! res lowres (vector-ref V2 lowV2))
                          #|   res[0..lowres] = (V1[0..lowV1-1] U (V2[0..lowV2])
                                                in nondecreasing order
                            AND 0 <= lowres<(vector-length res)
                            AND 0 <= lowV1<(vector-length V1)
                            AND 0 <= lowV2<(vector-length V2)
                            AND lowres = lowV1 + lowV2
                          |#
                          (set! lowV2 (add1 lowV2))
                          #|   res[0..lowres] = (V1[0..lowV1-1] U (V2[0..lowV2-1])
                                                in nondecreasing order
                            AND 0 <= lowres<(vector-length res)
                            AND 0 <= lowV1 <= (vector-length V1)
                            AND 0 <= lowV2 <= (vector-length V2)
                            AND lowres = lowV1 + lowV2-1
                          |#
                          (set! lowres (add1 lowres))
                          ;; INV
                          ))]
                   [(< lowV1 (vector-length V1))
                    (begin
                      #|   res[0..lowres-1] = (V1[0..lowV1-1] U (V2[0..lowV2-1])
                                              in nondecreasing order
                        AND 0 <= lowres < (vector-length res)
                        AND 0 <= lowV1 < (vector-length V1)
                        AND 0 <= lowV2 <= (vector-length V2)
                        AND lowres = lowV1 + lowV2
                        AND [0..(sub1 (vector-length V2))] is empty
                      |#
                      (vector-set! res lowres (vector-ref V1 lowV1))
                      #|   res[0..lowres] = (V1[0..lowV1] U (V2[0..lowV2-1])
                                            in nondecreasing order
                        AND 0 <= lowres < (vector-length res)
                        AND 0 <= lowV1 < (vector-length V1)
                        AND 0 <= lowV2 <= (vector-length V2)
                        AND lowres = lowV1 + lowV2
                      |#
                      (set! lowV1 (add1 lowV1))
                      #|   res[0..lowres] = (V1[0..lowV1-1] U (V2[0..lowV2])
                                            in nondecreasing order
                        AND 0 <= lowres < (vector-length res)
                        AND 0 <= lowV1 <= (vector-length V1)
                        AND 0 <= lowV2 <= (vector-length V2)
                        AND lowres = lowV1-1 + lowV2
                      |#
                      (set! lowres (add1 lowres))
                      ;; INV
                      )]
                   [else
                    (begin
                      #| res[0..lowres-1] = (V1[0..lowV1-1] U (V2[0..lowV2-1])
                                            in nondecreasing order
                        AND 0 <= lowres < (vector-length res)
                        AND 0 <= lowV1 <= (vector-length V1)
                        AND 0 <= lowV2 < (vector-length V2)
                        AND AND lowres = lowV1 + lowV2
                        AND [0..(sub1 (vector-length V1))] is empty
                      |#
                      (vector-set! res lowres (vector-ref V2 lowV2))
                      #| res[0..lowres] = (V1[0..lowV1-1] U (V2[0..lowV2])
                                          in nondecreasing order
                        AND 0 <= lowres < (vector-length res)
                        AND 0 <= lowV1 <= (vector-length V1)
                        AND 0 <= lowV2 < (vector-length V2)
                        AND lowres = lowV1 + lowV2
                      |#
                      (set! lowV2 (add1 lowV2))
                      #| res[0..lowres] = (V1[0..lowV1-1] U (V2[0..lowV2-1])
                                          in nondecreasing order
                        AND 0 <= lowres < (vector-length res)
                        AND 0 <= lowV1 <= (vector-length V1)
                        AND 0 <= lowV2 <= (vector-length V2)
                        AND lowres = lowV1 + lowV2-1
                      |#
                      (set! lowres (add1 lowres))
                      ;; INV
                      )]))
      #| INV AND losres > (sub1 (vector-length res))
         ==>     lowres = (vector-length res)
             AND lowV1 = (vector-length V1)
             AND lowV2 = (vector-length V2)
         ==> res[0..(vector-length res)-1]
                          = (V1[0..(vector-length V1)-1] U (V2[0..(vector-length V2)-1])
                            in nondecreasing order
      |#
      res
      #| Termination argument
         The state variable lowres starts at 0. Every time through the loop it is
         incremented by 1. Eventually, lowres becomes greater than
         (sub1 (vector-length res)) and the loop halts.
      |#
      )))


;; Tests for merge-vectors
(check-expect (merge-vectors V0 V0) V0)
(check-expect (merge-vectors V0 V1) V1)
(check-expect (merge-vectors V2 V0) V2)
(check-expect (merge-vectors V2 V1) (vector -8 -4 -2 1 2 3 10))
(check-expect (merge-vectors V1 V3) (vector 1 2 3 4 5 6))                    
                    

