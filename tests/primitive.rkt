#lang racket

(require "../src/lib.rkt")
(datatype Math
          (Num i64)
          (Sub Math Math))


(rewrite (Sub (Num a) (Num b)) (Num 0) :when ((equal? a b)))
(run-action! (Sub (Num 2) (Num 2)))

(rewrite (Sub (Num a) (Num b)) (Num (- a b)))
(run-action! (Sub (Num 4) (Num 3)))
(run1)


(print-size Sub)
(print-size Num)
(check (= (Sub (Num 4) (Num 3)) (Num 1)))
(check (= (Sub (Num 2) (Num 2)) (Num 0)))

(unless (null? (run-query (= (Sub (Num 4) (Num 3)) (Num 0))))
  (error "4-3=0"))

(unless (null? (run-query (= (Sub (Num 2) (Num 2)) (Num 1))))
  (error "2-2=1"))
