#lang racket/base

(require racket/list)
(require racket/match)

(provide uf-make-set uf-find uf-union! get-proof
         (rename-out [eclass-id-impl eclass-id])
         (struct-out union-record))

(define timestamp 0)
(define (get-timestamp)
  (define ts timestamp)
  (set! timestamp (add1 ts))
  ts)

;; ts and rev? are used for finding are constructing proofs
(struct union-record
  (;; from
   fr
   ;; to
   to
   ;; timestamp
   ts
   ;; Is the union operation applied reversely
   rev?)
  #:transparent)

(struct eclass
  (id
   rank
   parent
   ;; proof related
   proof-parent
   record)
  #:transparent)

(define (eclass-id-impl boxed-eclass)
  (eclass-id (unbox boxed-eclass)))

(define (uf-make-set x)
  (box (eclass x 0 #f #f #f)))

(define (uf-find x)
  (define p (eclass-parent (unbox x)))
  (if (not p)
      x
      (let ([leader (uf-find p)])
        (set-box! x (struct-copy eclass (unbox x)
                                 [parent leader]))
        leader)))

(define (uf-union! uf-mapper a b)
  (define a+ (hash-ref uf-mapper a))
  (define b+ (hash-ref uf-mapper b))
  (define res (uf-union!-impl a+ b+))
  (if (equal? res a+) a b))

(define (uf-union!-impl a b)
  (define pa (uf-find a))
  (define pb (uf-find b))
  (define pa-rank (eclass-rank (unbox pa)))
  (define pb-rank (eclass-rank (unbox pb)))

  (define (update pa pb rev?)
    (define record (union-record a b (get-timestamp) rev?))
    (set-box! pa (struct-copy eclass (unbox pa)
                              [parent pb]
                              [proof-parent pb]
                              [record record])))
  (cond [(equal? pa pb) pa]
        [(< pa-rank pb-rank)
         (update pa pb #f)
         pb]
        [(> pa-rank pb-rank)
         (update pb pa #t)
         pa]
        [else
         (update pb pa #t)
         (set-box! pa (struct-copy eclass (unbox pa)
                                   [rank (add1 (eclass-rank (unbox pa)))]))
         pa]))

(define (get-proof a b)
  (cond
    [(not (equal? (uf-find a) (uf-find b)))
     (error "Cannot find proof for inequivalent classes")]
    [(equal? a b) '()]
    [else (define (find-path a b lpath rpath)
            (define a* (unbox a))
            (define b* (unbox b))
            (if (equal? a b)
                (values lpath rpath)
                (if (< (eclass-rank a*)
                       (eclass-rank b*))

                    (find-path (eclass-proof-parent a*) b
                               (cons (eclass-record a*) lpath) rpath)
                    ;; For rpath, we need to flip the rev? flag
                    (let* ([record (eclass-record b*)]
                           [record+ (struct-copy union-record record
                                                 [rev? (not (union-record-rev? record))])])
                      (find-path a (eclass-proof-parent b*)
                                 lpath (cons record+ rpath))))))

          ;; Find path from a and b to their common ancestor
          (define-values (lpath rpath) (find-path a b '() '()))

          (define path (append lpath rpath))
          ;; The latest union operation must be part of the proof
          (define max-record (argmax union-record-ts path))
          (match-define (union-record fr to ts rev?) max-record)

          (define (build-proof fr to)
            ;; Jan 13: TODO: (list fr to) since jus is no longer part of proof-record
            (append (get-proof a fr) (list (cons (list fr to) rev?)) (get-proof to b)))
          (if (not rev?)
              (build-proof fr to)
              (build-proof to fr))]))

(module+ test
  (define xs
    (for/list ([i (in-inclusive-range 0 14)])
      (uf-make-set i)))

  (define (x i) (list-ref xs i))

  (uf-union!-impl (x 1) (x 8))
  (uf-union!-impl (x 7) (x 2))
  (uf-union!-impl (x 3) (x 13))
  (uf-union!-impl (x 7) (x 1))
  (uf-union!-impl (x 6) (x 7))
  (uf-union!-impl (x 9) (x 5))
  (uf-union!-impl (x 9) (x 3))
  (uf-union!-impl (x 14) (x 11))
  (uf-union!-impl (x 10) (x 4))
  (uf-union!-impl (x 12) (x 9))
  (uf-union!-impl (x 4) (x 11))
  (uf-union!-impl (x 10) (x 7))

  (get-proof (x 1) (x 4))
  (get-proof (x 4) (x 8))

  (get-proof (x 1) (x 14))
  )