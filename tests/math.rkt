#lang racket

(require "../src/lib.rkt")

(datatype Math
          (Diff Math Math)
          (Integral Math Math)

          (Add Math Math)
          (Sub Math Math)
          (Mul Math Math)
          (Div Math Math)
          (Pow Math Math)
          (Ln Math)
          (Sqrt Math)

          (Sin Math)
          (Cos Math)

          (Const Rational)
          (Var String))

(rewrite (Add a b) (Add b a))
(rewrite (Mul a b) (Mul b a))
(rewrite (Add a (Add b c)) (Add (Add a b) c))
(rewrite (Mul a (Mul b c)) (Mul (Mul a b) c))

(rewrite (Sub a b) (Add a (Mul (Const -1) b)))
;; (rewrite (Div a b) (Mul a (Pow b (Const -1))) :when ((is-not-zero b)))

(rewrite (Add a (Const 0)) a)
(rewrite (Mul a (Const 0)) (Const 0))
(rewrite (Mul a (Const 1)) a)

(rewrite (Sub a a) (Const 0))

(rewrite (Mul a (Add b c)) (Add (Mul a b) (Mul a c)))
(rewrite (Add (Mul a b) (Mul a c)) (Mul a (Add b c)))

(rewrite (Mul (Pow a b) (Pow a c)) (Pow a (Add b c)))
(rewrite (Pow x (Const 1)) x)
(rewrite (Pow x (Const 2)) (Mul x x))

(rewrite (Diff x (Add a b)) (Add (Diff x a) (Diff x b)))
(rewrite (Diff x (Mul a b)) (Add (Mul a (Diff x b)) (Mul b (Diff x a))))

(rewrite (Diff x (Sin x)) (Cos x))
(rewrite (Diff x (Cos x)) (Mul (Const -1) (Sin x)))

(rewrite (Integral (Const 1) x) x)
(rewrite (Integral (Cos x) x) (Sin x))
(rewrite (Integral (Sin x) x) (Mul (Const -1) (Cos x)))
(rewrite (Integral (Add f g) x) (Add (Integral f x) (Integral g x)))
(rewrite (Integral (Sub f g) x) (Sub (Integral f x) (Integral g x)))
(rewrite (Integral (Mul a b) x)
         (Sub (Mul a (Integral b x))
              (Integral (Mul (Diff x a) (Integral b x)) x)))

(run-action! (Integral (Ln (Var "x")) (Var "x")))
(run-action! (Integral (Add (Var "x") (Cos (Var "x"))) (Var "x")))
(run-action! (Integral (Mul (Cos (Var "x")) (Var "x")) (Var "x")))

(run-action! (Diff (Var "x") (Add (Const 1) (Mul (Const 2) (Var "x")))))
(run-action! (Diff (Var "x") (Sub (Pow (Var "x") (Const 3)) (Mul (Const 7) (Pow (Var "x") (Const 2))))))
(run-action! (Add (Mul (Var "y") (Add (Var "x") (Var "y"))) (Sub (Add (Var "x") (Const 2)) (Add (Var "x") (Var "x")))))
(run-action! (Div (Const 1)
                  (Sub (Div (Add (Const 1)
                                 (Sqrt (Var "five")))
                            (Const 2))
                       (Div (Sub (Const 1)
                                 (Sqrt (Var "five")))
                            (Const 2)))))
(run 3)
(print-size Add)
(print-size Mul)