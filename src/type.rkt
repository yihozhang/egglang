#lang racket/base

(require racket/match
         racket/function
         "union-find.rkt")

(provide i64 u64 String Rational unit
         semilattice
         sort term
         sort? term?
         show-base-type base-type-name
         literal? literal-type?
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
         False
         make-uf-mapper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; base types

(define i64 'i64)
(define u64 'u64)
(define String 'String)
(define Rational 'Rational)
(define unit '())
(struct semilattice (name dom bot join))
(struct sort (name) #:transparent)
(struct term (name) #:transparent)

(define (show-base-type type)
  (match type
    [(semilattice name dom bot join) `(semilattice ,name)]
    ['i64 'i64]
    ['u64 'u64]
    ['Rational 'Rational]
    ['String 'String]
    [(sort name) `(sort ,name)]
    [(term name) `(term ,name)]))

(define (base-type-name type)
  (match type
    [(semilattice name dom bot join) name]
    ['i64 'i64]
    ['u64 'u64]
    ['Rational 'Rational]
    ['String 'String]
    [(sort name) name]
    [(term name) name]))

(define (literal? l)
  (or (number? l)
      (rational? l)
      (string? l)))

(define (literal-type? type)
  (match type
    [(or 'i64 'u64 'Rational 'String) #t]
    [_ #f]))

(define (make-uf-mapper) (make-hash))

(define (new-value! uf-mapper type)
  (cond [(or (sort? type)
             (term? type))    (let* ([sym (gensym (base-type-name type))]
                                     [uf-val (uf-make-set sym)])
                                (hash-set! uf-mapper sym uf-val)
                                sym)]
        [(semilattice? type) (semilattice-bot type)]
        [(equal? unit type) '()]
        [else (raise (format "no default value for ~a" type))]))

(define (merge-fn! uf-mapper type vals)
  (cond [(sort? type)        (define canon-uf-val
                               (foldl (lambda (l acc) (uf-union! (hash-ref uf-mapper l) acc 'todo))
                                      (hash-ref uf-mapper (car vals))
                                      (cdr vals)))
                             (eclass-id canon-uf-val)]
        ;;  terms implement the choice operator of Datalog
        [(term? type) (car vals)]
        [(semilattice? type) (foldl (semilattice-join type) (car vals) (cdr vals))]
        [(equal? unit type) '()]
        [else (if (andmap (curry equal? (car vals)) (cdr vals))
                  (car vals)
                  (raise (format "merge function is not supported for ~a" type)))]))

(define (canonicalize uf-mapper type val)
  (cond [(sort? type) (eclass-id (uf-find (hash-ref uf-mapper val)))]
        [else val]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions

(struct function
  (name
   ;; a pair of input types and output type
   types
   constructor?
   )
  #:transparent)

(struct computed-function
  (name
   run)
  #:transparent)

(define (head-name head)
  (cond ([function? head] (function-name head))
        ([computed-function? head] (computed-function-name head))))

(define False (function 'False (cons '() unit) #f))

(define (show-computed-function func)
  (define name (computed-function-name func))
  `(computed ,name))

(define (show-function func)
  (define name (function-name func))
  (define in (function-input-types func))
  (define out (function-output-type func))
  (define head (if (function-constructor? func) 'constructor 'function))
  `(,head (,name ,@(map base-type-name in)) ,(base-type-name out)))

(define function-input-types (compose car function-types))
(define function-output-type (compose cdr function-types))
(define function-arity (compose add1 length function-input-types))
(define min-nat (semilattice 'min-nat u64 0 +))
