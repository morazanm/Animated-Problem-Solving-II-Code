;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname dfs-bfs) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

;; A (treeof X) is either:
;;   1. '()
;;   2. (make-node X (listof node))
(define-struct node (val subtrees))

#| FUNCTION TEMPLATES 

;;  Sample (treeof X)
(define TOX0 '())
(define TOX1 (make-node ... ...))
     ...
;; (treeof X) ... --> ...
;; Purpose:
(define (f-on-tox a-tox ...)
  (if (empty? a-tox)
      ...
      (f-on-node a-tox ...)))

;; Sample expressions for f-on-tox
(define TOX0-VAL ...)
(define TOX1-VAL ...)
     ...
;; Tests using sample computations for f-on-tox
(check-expect (f-on-tox TOX0 ...) TOX0-VAL)
(check-expect (f-on-tox TOX1 ...) TOX1-VAL)
     ...
;; Tests using sample values for f-on-tox
(check-expect (f-on-tox ... ...) ...)
     ...

;; Sample nodes
(define NODE0 (make-node ... ...))
     ...
;; node ... --> ...
;; Purpose:
(define (f-on-node a-node ...)
  (...(f-on-X (node-val a-node ...)
   ...(f-on-lox (node-subtrees a-node) ...)))

;; Sample expressions for f-on-node
(define NODE0-VAL ...)
     ...
;; Tests using sample computations for f-on-node
(check-expect (f-on-node NODE0 ...) NODE0-VAL)
     ...
;; Tests using sample values for f-on-node
(check-expect (f-on-node ... ...) ...)
     ...


;; Sample (listof X)
(define LOX0 '())
(define LOX1 ...)
     ...
;; a-lox ... --> ...
;; Purpose:
(define (f-on-lox a-lox ...)
  (if (empty? a-lox)
      ...
      ... (f-on-X (first a-lox) ...)
          (f-on-lox (rest a-lox)) ...))

;; Sample expressions for f-on-lox
(define LOX0-VAL ...)
(define LOX1-VAL ...)
     ...
;; Tests using sample computations for f-on-lox
(check-expect (f-on-lox LOX0 ...) LOX0-VAL)
(check-expect (f-on-lox LOX1 ...) LOX1-VAL)
     ...
;; Tests using sample values for f-on-lox
(check-expect (f-on-lox ... ...) ...)
     ...
|#

 
(define RANDOM-NUM-RANGE 1000000) 
(define MAX-NUM-SUBTREES 10)

;; natnum --> (treeof number)
;; Purpose: Create a random tree of numbers of the given maximum depth
(define (make-tonatnum d)
  (local [(define root-val (random RANDOM-NUM-RANGE))]
    (cond  [(= d 0) (make-node root-val '())]
           [else
            (make-node root-val
                       (build-list (random MAX-NUM-SUBTREES)
                                   (λ (i) (make-tonatnum (sub1 d)))))])))

;; Sample expressions for make-tonatnum
(define TON0-1 (local [(define root-val (random RANDOM-NUM-RANGE))]
                 (make-node root-val '())))
(define TON0-2 (local [(define root-val (random RANDOM-NUM-RANGE))]
                 (make-node root-val '())))
(define TON1 (local [(define root-val (random RANDOM-NUM-RANGE))]
               (make-node root-val
                          (build-list (random MAX-NUM-SUBTREES)
                                      (λ (i) (make-tonatnum (sub1 1)))))))
(define TON2 (local [(define root-val (random RANDOM-NUM-RANGE))]
               (make-node root-val
                          (build-list (random MAX-NUM-SUBTREES)
                                      (λ (i) (make-tonatnum (sub1 2)))))))

;; Tests using sample computations for make-tonatnum
(check-satisfied TON0-1   (λ (t) (and (integer? (node-val t))
                                      (>= (node-val t) 0)
                                      (empty? (node-subtrees t)))))
(check-satisfied TON0-2 (λ (t) (and (integer? (node-val t))
                                    (>= (node-val t) 0)
                                    (empty? (node-subtrees t)))))
(check-satisfied
 TON1
 (λ (t) (and (integer? (node-val t))
             (>= (node-val t) 0)
             (< (length (node-subtrees t)) MAX-NUM-SUBTREES)
             (andmap (λ (n)
                       (and (integer? (node-val n))
                            (>= (node-val t) 0)))
                     (node-subtrees t))
             (andmap
              (λ (n) (empty? (node-subtrees n)))
              (node-subtrees t)))))

;; Tests using sample values for make-tonatnum
(check-satisfied (make-tonatnum 0)
                 (λ (t) (and (integer? (node-val t))
                             (>= (node-val t) 0)
                             (empty? (node-subtrees t)))))
(check-satisfied
 (make-node 7 '())
 (λ (t) (and (integer? (node-val t))
             (>= (node-val t) 0)
             (empty? (node-subtrees t)))))

                                  


(define (lon-max a-lonatnum)
  (if (empty? a-lonatnum)
      0
      (local [(define max-of-rest (lon-max (rest a-lonatnum)))]
        (if (> (first a-lonatnum) max-of-rest)
            (first a-lonatnum)
            max-of-rest))))

(define (tox-depth a-tox)
  (if (or (empty? a-tox) (empty? (node-subtrees a-tox)))
      0
      (add1 (lon-max (map tox-depth (node-subtrees a-tox))))))

(define NODE10  (make-node 10  '()))
(define NODE3   (make-node  3  '()))
(define NODE87  (make-node 87  '()))
(define NODE-5  (make-node  -5 '()))
(define NODE0   (make-node   0 '()))
(define NODE66  (make-node  66 '()))
(define NODE44  (make-node  44 '()))
(define NODE47  (make-node  47 '()))
(define NODE850 (make-node 850 (list NODE10 NODE3)))
(define NODE235 (make-node 235 (list NODE87 NODE-5 NODE0)))
(define NODE23  (make-node  23 (list NODE44 NODE47)))
(define NODE-88 (make-node -88 (list NODE23)))
(define NODE600 (make-node 600 (list NODE850 NODE235 NODE66 NODE-88)))

(define T0 '())
(define T1 NODE10)
(define T2 NODE600)
(define T3 (make-tonatnum 7))

(check-satisfied T3 (λ (t) (<= (tox-depth t) 7)))
(check-satisfied (make-tonatnum 5)
                 (λ (t) (<= (tox-depth t) 5)))

;; number node --> Boolean
;; Purpose: Determine if given node contains given number
(define (node-dfs-contains? a-num a-node)
  (or (= a-num (node-val a-node))
      (ormap (λ (t) (node-dfs-contains? a-num t))
             (node-subtrees a-node))))

;; Sample expressions for node-dfs-contains?
(define NODE10-VAL1 (or (= 33 (node-val NODE10))
                        (ormap (λ (t) (node-dfs-contains? 33 t))
                               (node-subtrees NODE10))))
(define NODE10-VAL2 (or (= 10 (node-val NODE10))
                        (ormap (λ (t) (node-dfs-contains? 10 t))
                               (node-subtrees NODE10))))
(define NODE600-VAL (or (= -5 (node-val NODE600))
                        (ormap (λ (t) (node-dfs-contains? -5 t))
                               (node-subtrees NODE600))))

;; Tests using sample computations for node-dfs-contains?
(check-expect (node-dfs-contains? 33 NODE10)  NODE10-VAL1)
(check-expect (node-dfs-contains? 10 NODE10)  NODE10-VAL2)
(check-expect (node-dfs-contains? -5 NODE600) NODE600-VAL)

;; Tests using sample values for node-dfs-contains?
(check-satisfied (make-node 31
                            (list (make-node 45 '())
                                  (make-node 31 '())
                                  (make-node  7 '())))
                 (λ (t) (node-dfs-contains? 31 t)))
(check-satisfied (make-node 67
                            (list (make-node 45 '())
                                  (make-node 31 '())
                                  (make-node  7 '())))
                 (λ (t) (not (node-dfs-contains? 87 t))))

;; number (treeof number) --> Boolean
;; Purpose: Determine if the given number is in the given tree
(define (ton-dfs-contains? a-num a-ton)
  (and (not (empty? a-ton))
       (node-dfs-contains? a-num a-ton)))

;; Sample expressions for ton-dfs-contains?
(define T0-DFS-VAL (and (not (empty? T0))
                        (node-dfs-contains? 77 T1)))
(define T1-DFS-VAL (and (not (empty? T1))
                        (node-dfs-contains? 33 T1)))
(define T2-DFS-VAL (and (not (empty? T2))
                        (node-dfs-contains? 23 T2)))
(define T3-DFS-VAL (and (not (empty? T3))
                        (node-dfs-contains? 45 T3)))

;; Tests using sample computations for ton-dfs-contains?
(check-expect (ton-dfs-contains? 77 T0) T0-DFS-VAL)
(check-expect (ton-dfs-contains? 33 T1) T1-DFS-VAL)
(check-expect (ton-dfs-contains? 23 T2) T2-DFS-VAL)
(check-expect (ton-dfs-contains? 45 T3) T3-DFS-VAL)

;; Tests using sample values for ton-dfs-contains?
(check-satisfied (make-node 307759
                            (list (make-node 816392 '())
                                  (make-node 153333 '())
                                  (make-node 684270 '())))
                 (λ (t) (ton-dfs-contains? 153333 t)))
(check-satisfied (make-node 307759
                            (list (make-node 816392 '())
                                  (make-node 153333 '())
                                  (make-node 684270 '())))
                 (λ (t) (not (ton-dfs-contains? 6561 t))))


;; A queue of X, (qof X), is either:
;;  1. empty
;;  2. (cons X (qof X))


(define E-QUEUE '())

(define qempty? empty?)

;; Tests for qempty?
(check-expect (qempty? '())      #true)
(check-expect (qempty? '(a b c)) #false)

;; (listof X) (qof X) --> (qof X)
;; Purpose: Add the given list of X to the given queue of X
(define (enqueue a-lox a-qox) (append a-qox a-lox))

;; Tests for enqueue
(check-expect (enqueue '(8 d) '()) '(8 d))
(check-expect (enqueue '(d) '(a b c)) '(a b c d))
(check-expect (enqueue '(6 5 4) '(7)) '(7 6 5 4))

;; (qof X) --> X throws error
;; Purpose: Return first X of the given queue
(define (qfirst a-qox)
  (if (qempty? a-qox)
      (error "qfirst applied to an empty queue")
      (first a-qox)))

;; Tests for qfirst
(check-error  (qfirst '()) "qfirst applied to an empty queue")
(check-expect (qfirst '(a b c)) 'a)

;; (qof X) --> (qof X) throws error
;; Purpose: Return the rest of the given queue
(define (dequeue a-qox)
  (if (qempty? a-qox)
      (error "dequeue applied to an empty queue")
      (rest a-qox)))

;; Tests for qfirst
(check-error  (dequeue '()) "dequeue applied to an empty queue")
(check-expect (dequeue '(a b c)) '(b c))

(define QTON0 '())
(define QTON1 (list T1))
(define QTON2 (list T2 T1))

;; number (qof (treeof number)) --> Boolean
;; Purpose: Search the trees in the given queue for the given number
;; How: If the queue is empty or if the root value of
;;      the first tree in the queue equal the
;;      given number then stop. Otherwise, search
;;      for the number in a queue that contains all
;;      but the first tree in the given queue and
;;      the subtrees of  the first tree in the
;;      given queue.
(define (bfs-helper a-num a-qton)
  (and (not (qempty? a-qton))
       (or (= a-num (node-val (qfirst a-qton)))
           (local
             [(define newq (enqueue (node-subtrees (qfirst a-qton))
                                    (dequeue a-qton)))]
             (bfs-helper a-num newq)))))

;; Sample expressions for bfs-helper
(define QTON0-VAL
  (and (not (qempty? QTON0))
       (or (= 89 (node-val (qfirst QTON0)))
           (local
             [(define newq (enqueue (node-subtrees (qfirst QTON0))
                                    (dequeue QTON0)))]
             (bfs-helper 89 newq)))))
(define QTON1-VAL (and (not (qempty? QTON1))
                       (or (= 99 (node-val (qfirst QTON1)))
                           (local
                             [(define newq (enqueue (node-subtrees (qfirst QTON1))
                                                    (dequeue QTON1)))]
                             (bfs-helper 99 newq)))))
(define QTON2-VAL (and (not (qempty? QTON2))
                       (or (= 47 (node-val (qfirst QTON2)))
                           (local
                             [(define newq (enqueue (node-subtrees (qfirst QTON2))
                                                    (dequeue QTON2)))]
                             (bfs-helper 47 newq)))))

;; Tests using sample computations for bfs-helper
(check-expect (bfs-helper 89 QTON0) QTON0-VAL)
(check-expect (bfs-helper 99 QTON1) QTON1-VAL)
(check-expect (bfs-helper 47 QTON2) QTON2-VAL)

;; Tests using sample values for bfs-helper
(check-expect (bfs-helper
                 31
                 (list (make-node 768 '())))
              #false)
(check-expect (bfs-helper
               78
               (list (make-node 768 '())
                     (make-node
                      90
                      (list (make-node 1 '())
                            (make-node 78 '())))))
              #true)

;; number (treeof number) --> Boolean
;; Purpose: Determine if the given number is in the given tree
(define (ton-bfs-contains? a-num a-ton)
  (if (empty? a-ton)
      #false
      (bfs-helper a-num (enqueue (list a-ton) E-QUEUE))))

;;; Sample expressions for ton-bfs-contains?
(define T0-BFS-VAL #false)
(define T1-BFS-VAL (bfs-helper 33 (enqueue (list T1) E-QUEUE)))
(define T2-BFS-VAL (bfs-helper 23 (enqueue (list T2) E-QUEUE)))
(define T3-BFS-VAL (bfs-helper 45 (enqueue (list T3) E-QUEUE)))

;;; Tests using sample computations for ton-bfs-contains?
(check-expect (ton-bfs-contains? 77 T0) T0-BFS-VAL)
(check-expect (ton-bfs-contains? 33 T1) T1-BFS-VAL)
(check-expect (ton-bfs-contains? 23 T2) T2-BFS-VAL)
(check-expect (ton-bfs-contains? 45 T3) T3-BFS-VAL)

;;; Tests using sample values for ton-dfs-contains?
(check-satisfied (make-node 307759
                            (list (make-node 816392 '())
                                  (make-node 153333 '())
                                  (make-node 684270 '())))
                 (λ (t) (ton-bfs-contains? 153333 t)))
(check-satisfied (make-node 307759
                            (list (make-node 816392 '())
                                  (make-node 153333 '())
                                  (make-node 684270 '())))
                 (λ (t) (not (ton-bfs-contains? 6561 t))))


;(define T1-DFS (time (ton-bfs-contains?
;                      (list-ref (map (λ (t) (node-val t)) (node-subtrees T3))
;                                0)
;                      T3)))
;(define T2-DFS (time (ton-bfs-contains?
;                      (list-ref (map (λ (t) (node-val t)) (node-subtrees T3))
;                                (sub1 (length (node-subtrees T3))))
;                      T3)))

