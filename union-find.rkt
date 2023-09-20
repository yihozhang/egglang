#lang racket/base

(require data/gvector)
(provide uf-make-set uf-find uf-union!)

(define parent (make-gvector))

(struct eclass (id) #:prefab)

(define (uf-make-set)
  (define len (gvector-count parent))
  (gvector-add! parent len)
  (eclass len))

(define (uf-find c)
  (define (go x)
    (define p (gvector-ref parent x))
    (if (equal? p x)
        x
        (let ([leader (go p)])
          (gvector-set! parent x leader)
          leader)))

  (define x (eclass-id c))
  (eclass (go x)))

(define (uf-union! a b)
  (define pa (uf-find a))
  (define pb (uf-find b))
  (gvector-set! parent (eclass-id pb) (eclass-id pa))
  pa)
