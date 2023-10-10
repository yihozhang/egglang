#lang racket

(define timestamp 0)
(define (get-timestamp)
  (define ts timestamp)
  (set! timestamp (add1 ts))
  ts)

(struct union-record
  (fr
   to
   jus
   ts
   ;; Is the union operation applied reversely
   rev?)
  #:transparent)

(struct eclass
  (id
   rank
   parent
   record) #:transparent)

(define (ef-make-set x)
  (box (eclass x 0 #f #f)))

(define (ef-find x)
  (define p (eclass-parent (unbox x)))
  (if (not p)
      x
      (ef-find p)))

(define (ef-union! a b jus)
  (define pa (ef-find a))
  (define pb (ef-find b))
  (displayln pa)
  (displayln pb)
  (define pa-rank (eclass-rank (unbox pa)))
  (define pb-rank (eclass-rank (unbox pb)))
  (define (update pa pb rev?)
    (define record (union-record a b jus (get-timestamp) rev?))
    (set-box! pa (struct-copy eclass (unbox pa)
                              [parent pb]
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
    [(not (equal? (ef-find a) (ef-find b)))
     (error "Cannot find proof for inequivalent classes")]
    [(equal? a b) '()]
    [else (define (find-path a b lpath rpath)
            (define a* (unbox a))
            (define b* (unbox b))
            (if (equal? a b)
                (values lpath rpath)
                (if (< (eclass-rank a*)
                       (eclass-rank b*))

                    (find-path (eclass-parent a*) b
                               (cons (eclass-record a*) lpath) rpath)
                    ;; For rpath, we need to flip the rev? flag
                    (let* ([record (eclass-record b*)]
                           [record+ (struct-copy union-record record
                                                 [rev? (not (union-record-rev? record))])])
                      (find-path a (eclass-parent b*)
                                 lpath (cons record+ rpath))))))

          (define-values (lpath rpath) (find-path a b '() '()))

          (define path (append lpath rpath))
          (define max-record (argmax union-record-ts path))
          (match-define (union-record fr to jus ts rev?) max-record)

          (define (build-proof fr to)
            (append (get-proof a fr) (list (cons jus rev?)) (get-proof to b)))
          (if (not rev?)
              (build-proof fr to)
              (build-proof to fr))]))

(define xs
  (for/list ([i (in-inclusive-range 0 14)])
    (ef-make-set i)))

(define (x i) (list-ref xs i))

(ef-union! (x 1) (x 8) "(1, 8)")
(ef-union! (x 7) (x 2) "(7, 2)")
(ef-union! (x 3) (x 13) "(3, 13)")
(ef-union! (x 7) (x 1) "(7, 1)")
(ef-union! (x 6) (x 7) "(6, 7)")
(ef-union! (x 9) (x 5) "(9, 5)")
(ef-union! (x 9) (x 3) "(9, 3)")
(ef-union! (x 14) (x 11) "(14, 11)")
(ef-union! (x 10) (x 4) "(10, 4)")
(ef-union! (x 12) (x 9) "(12, 9)")
(ef-union! (x 4) (x 11) "(4, 11)")
(ef-union! (x 10) (x 7) "(10, 7)")

(get-proof (x 1) (x 4))

(get-proof (x 4) (x 8))

(get-proof (x 1) (x 14))
