#lang racket

(require "../src/lib.rkt")
(datatype Math
          (Num i64)
          (Sub Math Math))


(rewrite (Sub (Num a) (Num b)) (Num 0) :when ((= (equal? a b) #t)))
(run-action! (Sub (Num 2) (Num 2)))

; (rewrite (Sub (Num a) (Num b)) (Num (- a b)))
(run-action! (Sub (Num 4) (Num 3)))
(run1)


(print-table Sub)
(print-table Num)
