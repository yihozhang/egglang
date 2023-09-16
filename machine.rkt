#lang racket/base

;; interpreter


(struct vm-state
  (buffer ; a vector of arrays
   stack ; a stack of current values
   )
  :#mutable)

(define (instr-intersect indexs))
