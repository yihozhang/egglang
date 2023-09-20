#lang racket/base

(require racket/match data/union-find)

(provide i64 u64 unit semilattice sort
         show-base-type base-type? literal?
         function show-function function-name
         function-input-types function-output-type
         function-arity
         new-value! merge-fn!
         canonicalize
         False)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base types

(define i64 'i64)
(define u64 'u64)
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
    [(sort name) `(sort ,name)]))

(define (new-value! type)
  (cond [(sort? type)        (uf-new (gensym (sort-name type)))]
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

(define False (function 'False (cons '() unit)))

(define (show-function func)
  (define name (function-name func))
  (define in (car function-types))
  (define out (cdr function-types))
  `(function (,name ,@(map show-base-type in)) ,(show-base-type out)))

(define function-input-types (compose car function-types))
(define function-output-type (compose cdr function-types))
(define function-arity (compose add1 length function-input-types))
(define min-nat (semilattice 'min-nat u64 0 +))
