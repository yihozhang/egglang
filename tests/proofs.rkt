#lang racket

(require "../src/lib.rkt")


(datatype Math
          (Num Number)
          (Add Math Math)
          (Mul Math Math))


(rewrite (Add x y) (Add y x))
(rewrite (Add x (Add y z)) (Add (Add x y) z))

(run-action!
 (Add (Add (Num 1) (Num 2)) (Num 3)))

(for ([_ 5])
  (run1)
  (saturate '@))

(check (Add (Num 3) (Num 2)))
