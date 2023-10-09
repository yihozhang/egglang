#lang racket/base

(provide current-egraph current-ruleset
         register-function register-sort register-term register-rule
         egraph-functions egraph-sorts egraph-rulesets
         make-egraph show-egraph
         print-size print-table
         run1 run run-action! run-query
         table-length ;; TODO: export table-related APIs
         )
(require data/gvector
         data/bit-vector
         racket/set
         racket/function
         racket/match
         racket/list
         "core.rkt"
         "ast.rkt"
         "type.rkt"
         "union-find.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; egraph

(struct egraph
  (functions
   sorts
   terms
   rulesets))

(define (make-egraph)
  (egraph
   (make-hash)
   (make-gvector)
   (make-gvector)
   (make-hash (list (cons 'main (make-gvector))))))

(define (show-egraph egraph)
  (define function-asts
    (append*
     (for/list ([(fun table) (in-hash (egraph-functions egraph))])
       (cons (show-function fun)
             (show-table table)))))
  (define sort-asts
    (for/list ([sort (in-gvector (egraph-sorts egraph))])
      (show-base-type sort)))
  (append sort-asts function-asts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;; Registers a function
(define (register-function egraph function)
  (define functions (egraph-functions egraph))
  (define arity (function-arity function))
  (define name (function-name function))
  (hash-set! functions function (make-table name arity)))

;; Returns a table corresponding to the table
(define (lookup-function egraph function)
  (define functions (egraph-functions egraph))
  (hash-ref functions function))

;; Evaluates a funcion on the given input
(define (eval!-function egraph function args)
  (define table (lookup-function egraph function))
  (define access-pattern (append args '(#f)))
  (define tuples (table-get table access-pattern))
  (displayln function)
  (displayln args)
  (if (null? tuples)
      (let* ([otype (function-output-type function)]
             [new-val (new-value! otype)]
             [new-tuple (append args (list new-val))])
        (table-append! table new-tuple)
        new-val)
      (only (only tuples))))

;; Runs the `set` action on the given function
(define (set!-function egraph function args new-val)
  (define table (lookup-function egraph function))
  (define access-pattern (append args '(#f)))
  (define old-vals (table-get table access-pattern #:full-tuple? #f))
  (unless (<= (length old-vals) 1)
    (raise "Functional dependency is violated"))

  ;; figure out merge value
  (define val
    (if (not (null? old-vals))
        (let ([old-val (only (only old-vals))]
              [otype (function-output-type function)])
          (merge-fn! otype (list old-val new-val)))
        new-val))

  ;; remove the old tuple if exists
  (when (not (null? old-vals))
    (define old-tuple (append args (list (only (only old-vals)))))
    (table-remove! table old-tuple))

  ;; insert the new tuple
  (define new-tuple (append args (list val)))
  (table-append! table new-tuple))

;; Registers a sort
(define (register-sort egraph sort)
  (define sorts (egraph-sorts egraph))
  (gvector-add! sorts sort))

(define (register-term egraph term)
  (define terms (egraph-terms egraph))
  (gvector-add! terms term))

;; Registers a rule
(define (register-rule egraph rule #:ruleset ruleset)
  (define rulesets (egraph-rulesets egraph))
  (define rules (hash-ref! rulesets ruleset (thunk (make-gvector))))
  (gvector-add! rules rule))

(define current-egraph (make-parameter (make-egraph)))

(define current-ruleset (make-parameter 'main))

;; Runs an action
(define (run-action! action [egraph (current-egraph)])
  (define as (actions (list action)))
  (define core-actions (flatten-actions as))
  (run-core-actions! egraph core-actions))

(define (run-query query [egraph (current-egraph)])
  ;; TODO: For now, let's just rebuild the egraph manually
  ;; but what we really should do is to add a dirty bits to egraphs
  ;; and rebuild everytime needed
  (rebuild egraph)
  (define core-query (compile-query query egraph))
  (run-core-query egraph core-query))

;; Runs a ruleset for one iteration
(define (run1 [ruleset (current-ruleset)] [egraph (current-egraph)])
  (define rules (hash-ref (egraph-rulesets egraph) ruleset))
  (define compiled-rules (for/list ([rule (in-gvector rules)]) (compile rule egraph)))
  ;; search
  (define search-start-time (current-milliseconds))
  (define matches
    (map (curry run-core-query egraph)
         (map core-rule-query compiled-rules)))

  (for ([table (in-hash-values (egraph-functions egraph))])
    (drop-nonessential-indexes! table))

  ;; apply
  (define apply-start-time (current-milliseconds))
  (for ([rule (in-list compiled-rules)]
        [original-rule [in-gvector rules]]
        [ms (in-list matches)])
    (define actions (core-rule-actions rule))
    (unless (zero? (length ms))
      (displayln (format "Running ~a for ~a times" (show-rule original-rule) (length ms))))
    (for ([m ms])
      (run-core-actions! egraph actions m)))

  ;; rebuild
  (define rebuild-start-time (current-milliseconds))
  (rebuild egraph)
  (define rebuild-end-time (current-milliseconds))

  (list (cons 'search (- apply-start-time search-start-time))
        (cons 'apply (- rebuild-start-time apply-start-time))
        (cons 'rebuild (- rebuild-end-time rebuild-start-time)))
  )

(define (run n [ruleset (current-ruleset)] [egraph (current-egraph)])
  (define (combine-report r1 r2)
    (list (cons 'search (+ (cdr (assoc 'search r1)) (cdr (assoc 'search r2))))
          (cons 'apply (+ (cdr (assoc 'apply r1)) (cdr (assoc 'apply r2))))
          (cons 'rebuild (+ (cdr (assoc 'rebuild r1)) (cdr (assoc 'rebuild r2))))
          ))

  (define initial '((search . 0)
                    (apply . 0)
                    (rebuild . 0)))

  (define (do n result)
    (if (zero? n) result
        (let ([curr (run1 ruleset egraph)])
          (do (sub1 n) (combine-report curr result)))))

  (do n initial))

(define (run-core-query egraph query)
  (define (go egraph atoms m)
    (define (eval-arg arg) (if (symbol? arg)
                               (let ([val (assoc arg m)])
                                 (and val (cdr val)))
                               arg))
    (match atoms
      ['() (list m)]
      ;; core-check case
      [(cons (core-check arg) atoms+)
       (if (eval-arg arg)
           (go egraph atoms+ m)
           '())]
      ;; core-atom case
      [(cons (core-atom fun args) atoms+)
       (let ([pat (map eval-arg args)])
         (cond [(function? fun)
                ; function case
                (define table (lookup-function egraph fun))
                (define result (table-get table pat #:full-tuple? #t))
                (define (bound-and-proceed tuple)
                  (define-values (m+ valid?)
                    ;  `valid?` is used to handle non-linear patterns
                    (for/fold ([m m] [valid? #t])
                              ([arg args]
                               [tuple-val tuple]
                               #:when (symbol? arg)
                               #:break (not valid?))
                      (define instantiated (assoc arg m))
                      (cond [(not instantiated) (values (cons (cons arg tuple-val) m) #t)]
                            [(equal? (cdr instantiated) tuple-val) (values m #t)]
                            [else (values m #f)])
                      ))
                  (if valid?
                      (go egraph atoms+ m+)
                      '()))

                (append* (map bound-and-proceed result))]
               [(computed-function? fun)
                ;; primitive case
                (define-values (in-pats out-pats) (split-at-right pat 1))
                (define out-pat (only out-pats))
                ;; unless all arguments at an input position can be instantiated
                (unless (andmap identity in-pats)
                  (raise (format "Cannot execute query as ~a cannot be fully instantiated" (car atoms))))
                (define computed-val (apply (computed-function-run fun) in-pats))
                (cond [(not out-pat) (go egraph atoms+ (cons (cons (last args) computed-val) m))]
                      [(equal? out-pat computed-val) (go egraph atoms+ m)]
                      [else '()])]
               [else (raise (format "unsupported atom ~a" (car atoms)))]
               ))]
      ))

  (go egraph (core-query-atoms query) '()))

(define (run-core-actions! egraph actions [m '()])
  (define (eval-arg m arg)
    (if (symbol? arg)
        (cdr (assoc arg m))
        arg))
  (let go ([actions (core-actions-actions actions)]
           [m m])
    (match actions
      [(cons action rest)
       (match action
         [(core-let-atom-action var fun args)
          (define args+ (map (curry eval-arg m) args))
          (define val (cond [(function? fun) (eval!-function egraph fun args+)]
                            [(computed-function? fun)
                             (apply (computed-function-run fun) args+)]))
          (define m+ (cons (cons var val) m))
          (go rest m+)]
         [(core-let-val-action var val)
          (define val+ (eval-arg m val))
          (define m+ (cons (cons var val+) m))
          (go rest m+)]
         [(core-set-action fun args expr)
          (define args+ (map (curry eval-arg m) args))
          (define expr+ (eval-arg m expr))
          (set!-function egraph fun args+ expr+)
          (go rest m)]
         [(core-union-action v1 v2)
          (uf-union! (eval-arg m v1) (eval-arg m v2))
          (go rest m)])]
      ['() (void)])))

(define (rebuild egraph)
  (define functions (egraph-functions egraph))
  (define (rebuild-once)
    (for/sum ([fun (in-hash-keys functions)])
      (define arity (function-arity fun))
      ;; could be refactored into a function
      (define full-type (append (function-input-types fun)
                                (list (function-output-type fun))))
      (define update-count 0)
      (hash-update! functions fun
                    ;; TODO: this is slower than just throwing away the old database
                    ;; and build a new one.
                    ; (lambda (table)
                    ;   (define tab-list (table->list table))
                    ;   ;; canonicalize every tuple
                    ;   (for ([tuple tab-list])
                    ;     (define new-tuple (map canonicalize full-type tuple))
                    ;     (unless (equal? new-tuple tuple)
                    ;       (table-remove! table tuple)
                    ;       (table-append! table new-tuple))
                    ;     )

                    ;   ;; resolve conflicts
                    ;   (define key-index
                    ;     (let* ([sig (make-bit-vector arity #t)]
                    ;            [_ (bit-vector-set! sig (sub1 arity) #f)])
                    ;       (get-index! table sig)))
                    ;   (define otype (function-output-type fun))
                    ;   (define-values (to-adds to-removes)
                    ;     (for/fold ([to-adds '()]
                    ;                [to-removes '()])
                    ;               ([(key ids) (in-hash (index-hash key-index))]
                    ;                #:when (> (set-count ids) 1))
                    ;       (define old-tuples (set-map ids (curry table-get-by-id table)))
                    ;       (define old-vals (map last old-tuples))
                    ;       (define new-out-val (merge-fn! otype old-vals))
                    ;       (define tuple (list-set (first old-tuples) (sub1 arity) new-out-val))
                    ;       (values (cons tuple to-adds) (append old-tuples to-removes))
                    ;       ))
                    ;   (for-each (curry table-remove! table) to-removes)
                    ;   (for-each (curry table-append! table) to-adds)

                    ;   (define count (- (length to-removes) (length to-adds)))
                    ;   (set! update-count (+ update-count count))

                    ;   table)

                    (lambda (table)
                      (define tab-list (table->list table))
                      (define canon-tab-list
                        (for/list ([tuple tab-list])
                          (define new-tuple (map canonicalize full-type tuple))
                          (unless (equal? new-tuple tuple)
                            (set! update-count (add1 update-count)))
                          new-tuple))
                      (define grouped-by
                        (group-by (lambda (tuple) (drop-right tuple 1)) canon-tab-list))

                      (define new-table (make-table (table-name table) arity))
                      (define otype (function-output-type fun))
                      (define count
                        (for*/sum ([group (in-list grouped-by)])
                          (define len (length group))

                          (if (= len 1)
                              (table-append! new-table (first group))
                              (let ()
                                (define out-vals (map last group))
                                (define new-out-val (merge-fn! otype out-vals))
                                (define tuple (list-set (first group) (sub1 arity) new-out-val))
                                (table-append! new-table tuple)))

                          (sub1 len)))

                      (set! update-count (+ update-count count))
                      new-table)

                    )
      update-count))
  (let rebuild ([count 0])
    (define new (rebuild-once))
    (if (zero? new)
        count
        (rebuild (+ count new)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility commands

(define (print-size function (egraph (current-egraph)))
  (define table (hash-ref (egraph-functions egraph) function))
  (table-length table))

(define (print-table function (egraph (current-egraph)))
  (define table (hash-ref (egraph-functions egraph) function))
  (table->list table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access patterns and signatures

;; A signature is a bitvector mask
;; where #t means it's part of the index
;; A pattern is a signature converted to list
;; with every #t replaced with an actual value

;; TODO: don't use false for pattern, it is bad

;; Converts a pattern to a signature
(define (pattern->sig pat)
  (define len (length pat))
  (for/bit-vector #:length len
    ([b pat])
    (if b #t #f)))

;; Converts a full tuple to a pattern based on the signature
(define (apply-sig sig tuple)
  (for/list ([b (in-bit-vector sig)]
             [v (in-list tuple)])
    ;; trick: if b is #t, then use b, otherwise #f
    (and b v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; table

(define (only lst) (match lst [(list e) e]))

(struct table
  (name
   arity
   buffer
   indexes))

(define (show-table table)
  (define name (table-name table))
  (for/list ([tuple (table->list table)])
    (define-values (args out) (split-at-right tuple 1))
    `(set! (,name ,@args) ,@out)))

(define (make-table name arity)
  (define full-sig (make-bit-vector arity #t))
  (table name
         arity
         (make-gvector)
         (make-hash (list (cons full-sig (make-index full-sig))))))

;; Return a list of tuples
(define (table-get table access-pattern #:full-tuple? [full-tuple? #f])
  (define (compute-residual sig tuple)
    (for/list ([b sig]
               [v tuple]
               #:when (not b))
      v))

  (define signature (pattern->sig access-pattern))
  (define index (get-index! table signature))
  (define ids (index-ref index access-pattern))

  (define buffer (table-buffer table))
  (for/list ([id (in-set ids)])
    (define tuple (gvector-ref buffer id))
    (if full-tuple?
        tuple
        (compute-residual signature tuple))))

(define (table-append! table tuple)
  ;; add to the buffer
  (define buffer (table-buffer table))
  (define id (gvector-count buffer))
  (gvector-add! buffer tuple)

  ;; add to indices
  (define indexes (table-indexes table))
  (for ([(sig index) (in-hash indexes)])
    (index-add! index tuple id)))

(define (table-remove! table tuple)
  (define id (get-id-from-tuple table tuple))

  (define indexes (table-indexes table))
  (for ([(sig index) (in-hash indexes)])
    (index-remove! index tuple id)))

(define (table-length table)
  (define index (get-lookup-index table))
  (hash-count (index-hash index)))

(define (table->list table)
  (define index (get-lookup-index table))
  (index-keys index))

(define (drop-nonessential-indexes! table)
  (define full-sig (table-full-sig table))
  (define indexes (table-indexes table))
  (for ([sig (in-list (hash-keys indexes))]
        #:when (not (equal? sig full-sig)))
    (hash-remove! indexes sig)
    ))

;; Below are internal functions to table

(define (table-get-by-id table id)
  (gvector-ref (table-buffer table) id))

(define (get-id-from-tuple table tuple)
  (define lookup-index (get-lookup-index table))
  (define ids (index-ref lookup-index tuple))

  (when (> (set-count ids) 1) (raise "Set semantics is violated"))
  (if (set-empty? ids) #f
      (set-first ids)
      ))

(define (table-full-sig table)
  (define arity (table-arity table))
  (make-bit-vector arity #t))

(define (get-lookup-index table)
  (define full-sig (table-full-sig table))
  ;; such an index should always exist
  (get-index! table full-sig))

(define (get-index! table sig)
  (define indexes (table-indexes table))
  (define (create-index)
    (define lookup-index (get-lookup-index table))
    (define hash (make-hash))
    (for ([(tuple ids) (in-index lookup-index)])
      (when (> (set-count ids) 1)
        (raise "Set semantics is violated"))
      (define id (set-first ids))
      (set-add! (hash-ref! hash
                           (apply-sig sig tuple)
                           (thunk (mutable-set)))
                id))
    (index sig hash))
  (hash-ref! indexes sig create-index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indexes

(struct index
  (sig
   hash) #:transparent)

(define (make-index sig)
  (index sig
         (make-hash)))

;; Takes a pattern, returns all the ids for that pattern
(define (index-ref index access-pattern [default (thunk (mutable-set))])
  (hash-ref (index-hash index) access-pattern default))

(define (index-add! index tuple id)
  (define hash (index-hash index))
  (define sig (index-sig index))
  (define pat (apply-sig sig tuple))
  (define ids (hash-ref! hash pat (thunk (mutable-set))))
  (set-add! ids id))

(define (index-remove! index tuple id)
  (define hash (index-hash index))
  (define sig (index-sig index))
  (define pat (apply-sig sig tuple))
  (define ids (hash-ref hash pat))
  (set-remove! ids id)
  (when (zero? (set-count ids))
    (hash-remove! hash pat)
    ))

(define (index-keys index)
  (define hash (index-hash index))
  (hash-keys hash))

(define in-index (compose in-hash index-hash))
