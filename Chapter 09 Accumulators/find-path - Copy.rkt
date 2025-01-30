;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname find-path) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))


#|
A node is a structure, (make-node symbol (listof symbol)),
where the symbol is the node's name and the (listof symbol)
is a list of the names of the node's neighbors on outgoing edges.
|#
(define-struct node (name neighs))

;; Nodes for G1
(define NODEA (make-node 'A '(B D F)))
(define NODEB (make-node 'B '(C)))
(define NODEC (make-node 'C '(Z)))
(define NODED (make-node 'D '(E)))
(define NODEE (make-node 'E '(Z)))
(define NODEF (make-node 'F '(G)))
(define NODEG (make-node 'G '(Z)))
(define NODEZ (make-node 'Z '()))

;; Nodes for G2
(define NODEC2 (make-node 'C '()))

;; Nodes for G3
(define NODEC3 (make-node 'C '(A F Z)))
(define NODEE3 (make-node 'E '(A Z)))
(define NODEG3 (make-node 'G '(A Z)))

#|
Template for functions on a node
node ... --> ...
Purpose:
(define (f-on-node a-node ...)
  ...(node-name a-node)...(node-neighs a-node)...)

     ;; Sample expressions for f-on-node
     (define NODEA-VAL ...)
             ...

     ;; Tests using sample computations for f-on-node
     (check-expect (f-on-node NODEA ...) NODEA-VAL)
                   \vdotss
                   
     ;; Tests using sample values for f-on-node
     (check-expect (f-on-node ... ...) ...)
                   ...
|#

#| A graph is a nonempty (listof node). |#

(define G1 (list NODEA NODEB NODEC NODED
                 NODEE NODEF NODEG NODEZ))

(define G2 (list NODEA NODEB NODEC2 NODED
                 NODEE NODEF NODEG  NODEZ))

(define G3 (list NODEA  NODEB NODEC3 NODED
                 NODEE3 NODEF NODEG3 NODEZ))

#|
Template for functions on a graph
graph ... --> ...
Purpose:
(define (f-on-graph a-graph ...)
  (if (empty? (rest a-graph))
      (f-on-node (first a-graph) ...)
      (...(f-on-node (first a-graph)...)...
       ...(f-on-graph (rest a-graph)...)...)))

;; Sample expressions for f-on-graph
(define FONG-G1-VAL ...)
(define FONG-G2-VAL ...)
(define FONG-G13-VAL ...)
     ...

;; Tests using sample computations for f-on-graph
(check-expect (f-on-graph G1 ...) FONG-G1-VAL)
(check-expect (f-on-graph G2 ...) FONG-G2-VAL)
(check-expect (f-on-graph G3 ...) FONG-G3-VAL)
     ...
;; Tests using sample values for f-on-graph
(check-expect (f-on-graph ... ...) ...)
     ...
|#

#|
A path is either:
  1. (listof symbol)
  2. 'no-path
|#

#| VERSION WITHOUT BACKTRACKING (not included in Animated Problem Solving II)

(define (find-path-from-any-neighbor a-graph neighs b)
  (if (empty? neighs)
      'no-path
      (local [(define path-from-first (find-path a-graph (first neighs) b))]
        (if (not (eq? path-from-first 'no-path))
            path-from-first
            (find-path-from-any-neighbor a-graph (rest neighs) b)))))
            

(define (find-path a-graph start b)
  (if (eq? start b)
      (list b)
      (local [(define a-neighs (node-neighs (first (filter
                                                    (λ (n) (eq? (node-name n) a))
                                                    a-graph))))
              (define path-from-neigh (find-path-from-any-neighbor a-graph a-neighs b))]
        (if (eq? path-from-neigh 'no-path)
            'no-path
            (cons start path-from-neigh)))))
                
(check-expect (find-path G1 'A 'Z) '(A B C Z))
(check-expect (find-path G2 'C 'Z) 'no-path)
;;(check-expect (find-path G3 'A 'Z) '(A B C Z)) ;; infinite recursion
|#

;; graph (listof symbol) symbol (listof symbol) --> path
;; Purpose: Find a path from any neighbor to the given node in the given graph without going through any visited neighbors
;; Accumulator Invariant: visited = nodes whose neighbors are part of the search
;; Assumptions: Given neighbors and node name are in the given graph
;;              None of the given neighbors are in visited
(define (find-path-from-neighbors a-graph neighs end visited)
  (if (empty? neighs)
      'no-path
      (local [(define path-from-first (find-path-acc a-graph
                                                     (first neighs)
                                                     end
                                                     visited))]
        (if (not (eq? path-from-first 'no-path))
            path-from-first
            (find-path-from-neighbors a-graph
                                      (rest neighs)
                                      end
                                      visited)))))


;; graph symbol symbol (listof symbol) --> path
;; Purpose: Find a path between given node names in given graph without going through any visited neighbors
;; Accumulator Invariant: visited = nodes whose neighbors are part of the search
;; Assumption: Given node names are in the given graph
(define (find-path-acc a-graph start end visited)
  (if (eq? start end)
      (list end)
      (local [(define new-visited (cons start visited))
              (define start-unvisited-neighs
                (filter
                 (λ (s) (not (member? s new-visited)))
                 (node-neighs (first (filter
                                      (λ (n) (eq? (node-name n) start))
                                      a-graph)))))
              (define path-from-neigh (find-path-from-neighbors
                                       a-graph
                                       start-unvisited-neighs
                                       end
                                       new-visited))]
        (if (eq? path-from-neigh 'no-path)
            'no-path
            (cons start path-from-neigh)))))

;; Sample expressions for find-path-acc
(define FPACC-AA-G1 (list 'A))
(define FPACC-AZ-G1 (local [(define new-visited (cons 'A '(C)))
                            (define start-unvisited-neighs
                              (filter
                               (λ (s) (not (member? s new-visited)))
                               (node-neighs (first (filter
                                                    (λ (n) (eq? (node-name n) 'A))
                                                    G1)))))
                            (define path-from-neigh (find-path-from-neighbors
                                                     G1
                                                     start-unvisited-neighs
                                                     'Z
                                                     new-visited))]
                      (if (eq? path-from-neigh 'no-path)
                          'no-path
                          (cons 'A path-from-neigh))))

(define FPACC-ZB-G3 (local [(define new-visited (cons 'Z '()))
                            (define start-unvisited-neighs
                              (filter
                               (λ (s) (not (member? s new-visited)))
                               (node-neighs (first (filter
                                                    (λ (n) (eq? (node-name n) 'Z))
                                                    G3)))))
                            (define path-from-neigh (find-path-from-neighbors
                                                     G3
                                                     start-unvisited-neighs
                                                     'B
                                                     new-visited))]
                      (if (eq? path-from-neigh 'no-path)
                          'no-path
                          (cons 'Z path-from-neigh))))

;; Test using sample computations for find-path-acc
(check-expect (find-path-acc G1 'A 'A '())  FPACC-AA-G1)
(check-expect (find-path-acc G1 'A 'Z '(C)) FPACC-AZ-G1)
(check-expect (find-path-acc G3 'Z 'B '())  FPACC-ZB-G3)

;; Test using sample values for find-path-acc
(check-expect (find-path-acc G2 'A 'Z '(B D)) (list 'A 'F 'G 'Z))
(check-expect (find-path-acc G3 'F 'Z '(G)) 'no-path)

;; graph symbol symbol --> path
;; Purpose: Find a path between given node names in given graph
;; Assumption: Given node names are in the given graph
(define (find-path a-graph start end)
  (find-path-acc a-graph start end '()))

;; Sample expressions for find-path
(define FP-AZ-G1 (find-path-acc G1 'A 'Z '()))
(define FP-AZ-G2 (find-path-acc G2 'C 'Z '()))
(define FP-AZ-G3 (find-path-acc G3 'A 'Z '()))

;; Test using sample computations for find-path
(check-expect (find-path G1 'A 'Z) FP-AZ-G1)
(check-expect (find-path G2 'C 'Z) FP-AZ-G2)
(check-expect (find-path G3 'A 'Z) FP-AZ-G3) ;; not infinite recursion

;; Test using sample values for find-path
(check-expect (find-path G1 'Z 'A) 'no-path)
(check-expect (find-path G2 'F 'Z) '(F G Z))
(check-expect (find-path G3 'A 'Z) '(A B C F G Z)) ;; not shortest path


;; Sample Expressions for find-path-from-neighbors
(define FPN-ZA-G1 'no-path)
(define FPN-AZ-G2 (local [(define path-from-first (find-path-acc G2
                                                                 (first '(B D F))
                                                                 'Z
                                                                 '(A)))]
                    (if (not (eq? path-from-first 'no-path))
                        path-from-first
                        (find-path-from-neighbors G2
                                                  (rest '(B D F))
                                                  'Z
                                                  '(A)))))
(define FPN-FA-G3 (local [(define path-from-first (find-path-acc G3
                                                                 (first '(E))
                                                                 'A
                                                                 '(F)))]
                    (if (not (eq? path-from-first 'no-path))
                        path-from-first
                        (find-path-from-neighbors G3
                                                  (rest '(E))
                                                  'A
                                                  '(F)))))

;; Tests using sample computations for find-path-from-neighbors
(check-expect (find-path-from-neighbors G1 '()      'A '())  FPN-ZA-G1)
(check-expect (find-path-from-neighbors G2 '(B D E) 'Z '(A)) FPN-AZ-G2)
(check-expect (find-path-from-neighbors G3 '(E)     'A '(E)) FPN-FA-G3)

;; Tests using sample values for find-path-from-neighbors
(check-expect (find-path-from-neighbors G1 '(C) 'E '(B)) 'no-path)
(check-expect (find-path-from-neighbors G3 '(G) 'C '(F)) '(G A B C))
