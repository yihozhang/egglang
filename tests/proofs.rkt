#lang racket

(require "../src/lib.rkt")

(require "../src/proofs.rkt")
(datatype Math
          (Num Number)
          (Add Math Math)
          ;; TODO: think about how we handle non-constructor functions.
          ;; Do they also have a term representation?
          (e))


(rewrite (Add x y) (Add y x))
(rewrite (Add x (Add y z)) (Add (Add x y) z))

(run-action!
 (Add (Add (Num 1) (Num 2)) (Num 3)))

(for ([_ 5])
  (run1)
  (saturate '@))

(check (Add (Num 3) (Num 2)))

; (run-action! (set (e) (Add@ (Num@ 3) (Num@ 2))))
(run-action! (union! (e) (Add (Num 3) (Num 2))))

; TODO: this is ugly
(define-values (eid _updated) (eval!-function (current-egraph) e '()))
(define repr (get-repr-term (current-egraph) Math eid))
(get-existence-proof (egraph-proof-manager (current-egraph)) repr)