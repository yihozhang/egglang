#lang racket/base

(require racket/match
         "union-find.rkt")

(provide i64 u64 String Rational unit
         semilattice
         sort
         show-base-type base-type-name base-type? literal?
         ;; function related
         function
         function? show-function function-name
         function-input-types function-output-type
         function-arity
         ;; computed function related
         computed-function
         computed-function?
         computed-function-run
         ;; utility
         head-name
         ;; make-set and merge
         new-value! merge-fn!
         canonicalize
         False)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base types

(define i64 'i64)
(define u64 'u64)
(define String 'String)
(define Rational 'Rational)
(define unit '())
(struct semilattice (name dom bot join))
(struct sort (name) #:transparent)

(define (literal? l)
  (or (number? l)
      ))

(define (base-type? type)
  (match type
    [(or 'i64) #t]
    [_ #f]))

(define (show-base-type type)
  (match type
    [(semilattice name dom bot join) `(semilattice ,name)]
    ['i64 'i64]
    ['u64 'u64]
    ['Rational 'Rational]
    ['String 'String]
    [(sort name) `(sort ,name)]))

(define (base-type-name type)
  (match type
    [(semilattice name dom bot join) name]
    ['i64 'i64]
    ['u64 'u64]
    [(sort name) name]))


(define (new-value! type)
  (cond [(sort? type)        (uf-make-set)]
        [(semilattice? type) (semilattice-bot type)]
        [else (raise (format "no default value for ~a" type))]))

(define (merge-fn! type vals)
  (cond [(sort? type)        (foldl uf-union! (car vals) (cdr vals))
                             (uf-find (car vals))]
        [(semilattice? type) (foldl (semilattice-join type) (car vals) (cdr vals))]
        [else (raise (format "merge function is not supported for ~a" type))]))

(define (canonicalize type val)
  (cond [(sort? type) (uf-find val)]
        [else val]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(struct function
  (name
   ;; a pair of input types and output type
   types
   )
  #:transparent)

(struct computed-function
  (name
   run)
  #:transparent)

(define (head-name head)
  (cond ([function? head] (function-name head))
        ([computed-function? head] (computed-function-name head))))

(define False (function 'False (cons '() unit)))

(define (show-computed-function func)
  (define name (computed-function-name func))
  `(computed ,name))

(define (show-function func)
  (define name (function-name func))
  (define in (function-input-types func))
  (define out (function-output-type func))
  `(function (,name ,@(map base-type-name in)) ,(base-type-name out)))

(define function-input-types (compose car function-types))
(define function-output-type (compose cdr function-types))
(define function-arity (compose add1 length function-input-types))
(define min-nat (semilattice 'min-nat u64 0 +))
