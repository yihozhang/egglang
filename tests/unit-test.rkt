#lang racket

(require "../src/lib.rkt")

(sort Math)
(declare-const zero Math)
(define-const zero2 Math (zero))

(print-table zero)
(print-table zero2)
(check (= (zero) (zero2)))