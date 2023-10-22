#lang racket

(require "../src/lib.rkt")

(datatype Math (Num i64) (Add Math Math))

(ruleset a
         (rule () ((Num 1))))

(ruleset b
         (rule () ((Num 2))))

;; TODO refactor into own lib
(define-syntax assert
  (syntax-rules ()
    ([assert b] (unless b (error 'assert "~a" (quote b))))))

(assert (= (print-size Num) 0))
(run1)
(assert (= (print-size Num) 0))
(run1 'a)
(assert (= (print-size Num) 1))
(run1 'b)
(assert (= (print-size Num) 2))
