#lang racket

(require "../src/lib.rkt")

;; NOTE: This file contains several unsafe operations
(term Value@)
(term Variable@)
(datatype Term
          (Val Value@)
          (Var Variable@)
          (Add Term Term)
          (Eq Term Term)
          (App Term Term)
          (Lam Variable@ Term)
          (Let Variable@ Term Term)
          (Fix Variable@ Term)
          (If Term Term Term))

(define != (compose not equal?))

;; Value
(function (Num@ i64) Value@)
(function (True@) Value@)
(function (False@) Value@)

;; Var
(function (V@ String) Variable@)
(function (From@ Term) Variable@)

; All free variables are free,
; but some free variables are more free than others
; Freer variables will only contain variables
; that will affect the evaluation
; of the corresponding term
; e.g., x is free in x - x, but not freer in x - x
(define VarSet (semilattice 'VarSet '(set-of varset) (set) set-intersect))
(function (freer Term) VarSet)
(ruleset freer
         (rule ((= e (Val v)))
               ((set! (freer e) (set))))
         (rule ((= e (Var v)))
               ((set! (freer e) (set v))))
         (rule ((= e (Add e1 e2))
                (= (freer e1) fv1)
                (= (freer e2) fv2))
               ((set! (freer e) (set-union fv1 fv2))))
         (rule ((= e (Eq e1 e2))
                (= (freer e1) fv1)
                (= (freer e2) fv2))
               ((set! (freer e) (set-union fv1 fv2))))
         (rule ((= e (App e1 e2))
                (= (freer e1) fv1)
                (= (freer e2) fv2))
               ((set! (freer e) (set-union fv1 fv2))))
         (rule ((= e (Lam var body))
                (= (freer body) fv))
               ((set! (freer e) (set-remove fv var))))
         (rule ((= e (Let var e1 e2))
                (= (freer e1) fv1)
                (= (freer e2) fv2))
               ((set! (freer e) (set-union (set-remove fv1 var) fv2))))
         (rule ((= e (Fix var body))
                (= (freer body) fv))
               ((set! (freer e) (set-remove fv var))))
         (rule ((= e (If c e1 e2))
                (= (freer c) fv1)
                (= (freer e1) fv2)
                (= (freer e2) fv3))
               ((set! (freer e) (set-union fv1 (set-union fv2 fv3)))))
         )

;; START evals-to
(function (evals-to Term) Value@)
(ruleset evals-to
         (rule ((= e (Val val)))
               ((set! (evals-to e) val)))
         (rule ((= e (Add a b))
                (= (Num@ va) (evals-to a))
                (= (Num@ vb) (evals-to b)))
               ((set! (evals-to e) (Num@ (+ va vb)))))
         (rule ((= e (Eq a b))
                (= va (evals-to a))
                (= vb (evals-to b))
                (= va vb))
               ((set! (evals-to e) (True@))))
         (rule ((= e (Eq a b))
                (= va (evals-to a))
                (= vb (evals-to b))
                (!= va vb))
               ((set! (evals-to e) (False@))))

         (rule ((= v (evals-to e)))
               ((union! e (Val v))))
         )
;; END evals-to

; if-true
(rewrite (If (Val (True@)) then else) then)
; if-false
(rewrite (If (Val (False@)) then else) else)
; if-elim
(rule ((= term (If (Eq (Var x) e) then else)))
      ((Let x e then)
       (Let x e else)))
(rewrite (If (Eq (Var x) e) then else) else
         :when ((= (Let x e then) (Let x e else))))

; add-comm
(rewrite (Add a b) (Add b a))
; add-assoc
(rewrite (Add (Add a b) c) (Add a (Add b c)))
; eq-comm
(rewrite (Eq a b) (Eq b a))

; fix
(rewrite (Fix v e) (Let v (Fix v e) e))
; beta
(rewrite (App (Lam v body) e) (Let v e body))
; let-app
(rewrite (Let v e (App a b)) (App (Let v e a) (Let v e b)))
; let-add
(rewrite (Let v e (Add a b)) (Add (Let v e a) (Let v e b)))
; let-eq
(rewrite (Let v e (Eq a b)) (Eq (Let v e a) (Let v e b)))
; let-const
(rewrite (Let v e c) c :when ((= const (evals-to c))))
; let-if
(rewrite (Let v e (If cond then else))
         (If (Let v e cond) (Let v e then) (Let v e else)))
; let-var-same
(rewrite (Let v1 e (Var v1)) e)
; let-var-diff
(rewrite (Let v1 e (Var v2)) (Var v2) :when ((!= v1 v2)))
; let-lam-same
(rewrite (Let v1 e (Lam v1 body)) (Lam v1 body))
; let-lam-diff
(rewrite (Let v1 e (Lam v2 body)) (Lam v2 (Let v1 e body))
         :when ((!= v1 v2)
                (= fvs (freer e))
                (not (set-member? fvs v2))))
(rule ((= expr (Let v1 e (Lam v2 body)))
       (!= v1 v2)
       (= fvs (freer e))
       (set-member? fvs v2))
      ((let expr@ (Term:=>@ expr))
       (union! expr (Lam (From@ expr) (Let v1 e (Let v2 (Var (From@ expr)) body))))))

(define (run-schedule n)
  (for ([_ n])
    (run1)
    (saturate 'evals-to)
    (run1 'freer)
    (saturate '@)))

;; lambda_under
(define-const e Term
  (Lam (V@ "x")
       (Add (Val (Num@ 4))
            (App (Lam (V@ "y") (Var (V@ "y"))) (Val (Num@ 4))))))
(run-schedule 10)
(check (= (e) (Lam (V@ "x") (Val (Num@ 8)))))

;; lambda_if_elim
(define-const e2 Term (If (Eq (Var (V@ "a")) (Var (V@ "b")))
                          (Add (Var (V@ "a")) (Var (V@ "a")))
                          (Add (Var (V@ "a")) (Var (V@ "b")))))
(run-schedule 10)
(check (= (e2) (Add (Var (V@ "a")) (Var (V@ "b")))))

;; lambda_let_simple
(define-const e3 Term (Let (V@ "x") (Val (Num@ 0))
                           (Let (V@ "y") (Val (Num@ 1))
                                (Add (Var (V@ "x")) (Var (V@ "y"))))))
(run-schedule 10)
(check (= (e3) (Val (Num@ 1))))

;; lambda_capture
(define-const e4 Term (Let (V@ "x") (Val (Num@ 1))
                           (Lam (V@ "x") (Val (V@ "x")))))
(run-schedule 10)
(define-const e4-wrong Term (Lam (V@ "x") (Val (Num@ 1))))
(check (!= (e4) (e4-wrong)))

;; lambda_capture_free
(define-const e5 Term  (Let (V@ "y") (Add (Var (V@ "x")) (Var (V@ "x")))
                            (Lam (V@ "x") (Var (V@ "y")))))
(run-schedule 10)
(check (set-member? (freer (Lam (V@ "x") (Var (V@ "y")))) (V@ "y")))
(define-const e5-wrong Term (Lam (V@ "x") (Add (Var (V@ "x")) (Var (V@ "x")))))
(check (!= (e5) (e5-wrong)))

;; lambda_closure_not_seven
(define-const e6 Term
  (Let (V@ "five") (Val (Num@ 5))
       (Let (V@ "add-five") (Lam (V@ "x") (Add (Var (V@ "x")) (Var (V@ "five"))))
            (Let (V@ "five") (Val (Num@ 6))
                 (App (Var (V@ "add-five")) (Val (Num@ 1)))))))
(run-schedule 10)
(define-const e6-wrong Term (Val (Num@ 7)))
(check (!= (e6) (e6-wrong)))
(check (= (e6) (Val (Num@ 6))))

;; lambda_compose
(define-const e7 Term
  (Let (V@ "compose") (Lam (V@ "f")
                           (Lam (V@ "g")
                                (Lam (V@ "x") (App (Var (V@ "f"))
                                                   (App (Var (V@ "g"))
                                                        (Var (V@ "x")))))))
       (Let (V@ "add1") (Lam (V@ "y") (Add (Var (V@ "y")) (Val (Num@ 1))))
            (App (App (Var (V@ "compose")) (Var (V@ "add1"))) (Var (V@ "add1"))))))
(run-schedule 20)
(run-query (= (e7) (Lam (V@ "x") (Add (Var (V@ "x")) (Val (Num@ 2))))))

;; lambda_if_simple
(define-const e10 Term (If (Eq (Val (Num@ 1)) (Val (Num@ 1))) (Val (Num@ 7)) (Val (Num@ 9))))
(run-schedule 10)
(check (= (e10) (Val (Num@ 7))))

; lambda_compose_many
(define-const e11 Term
  (Let (V@ "compose") (Lam (V@ "f") (Lam (V@ "g") (Lam (V@ "x") (App (Var (V@ "f"))
                                                                     (App (Var (V@ "g")) (Var (V@ "x")))))))
       (Let (V@ "add1") (Lam (V@ "y") (Add (Var (V@ "y")) (Val (Num@ 1))))
            (App (App (Var (V@ "compose")) (Var (V@ "add1")))
                 (App (App (Var (V@ "compose")) (Var (V@ "add1")))
                      (App (App (Var (V@ "compose")) (Var (V@ "add1")))
                           (App (App (Var (V@ "compose")) (Var (V@ "add1")))
                                (App (App (Var (V@ "compose")) (Var (V@ "add1")))
                                     (App (App (Var (V@ "compose")) (Var (V@ "add1")))
                                          (Var (V@ "add1")))))))))))
(run-schedule 30)
(check (= (e11) (Lam x (Add (Var x) (Val (Num@ 7))))))

;; lambda_if
(define-const e8 Term
  (Let (V@ "zeroone") (Lam (V@ "x") (If (Eq (Var (V@ "x")) (Val (Num@ 0)))
                                        (Val (Num@ 0))
                                        (Val (Num@ 1)))
                           )
       (Add (App (Var (V@ "zeroone")) (Val (Num@ 0)))
            (App (Var (V@ "zeroone")) (Val (Num@ 10))))))
(run-schedule 10)
(check (= (e8) (Val (Num@ 1))))
