#lang racket/base

(provide uf-make-set uf-find uf-union!)

(struct eclass (id rank parent) #:transparent)

(define (uf-make-set x)
  (box (eclass x 0 #f)))

(define (uf-find x)
  (define p (eclass-parent (unbox x)))
  (if (not p)
      x
      (let ([leader (uf-find p)])
        (set-box! x (struct-copy eclass (unbox x)
                                 [parent leader]))
        leader)))

(define (uf-union! a b)
  (define pa (uf-find a))
  (define pb (uf-find b))
  (define pa-rank (eclass-rank (unbox pa)))
  (define pb-rank (eclass-rank (unbox pb)))
  (define (update pa pb)
    (set-box! pa (struct-copy eclass (unbox pa)
                              [parent pb])))
  (cond [(equal? pa pb) pa]
        [(< pa-rank pb-rank)
         (update pa pb)
         pb]
        [(> pa-rank pb-rank)
         (update pb pa)
         pa]
        [else
         (update pb pa)
         (set-box! pa (struct-copy eclass (unbox pa)
                                   [rank (eclass-rank (unbox pa))]))
         pa]))
