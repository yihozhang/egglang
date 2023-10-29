#lang racket/base

(provide current-egraph current-ruleset
         ;; functions
         register-function register-constructor
         egraph-functions
         ;; sort and term
         register-sort register-term register-sort-term-pair get-term-or-id
         get-=>@ get-@=>
         egraph-sorts
         ;; rules
         register-rule
         egraph-rulesets
         make-egraph show-egraph
         print-size print-table
         run1 run run-action! run-query saturate
         table-length ;; TODO: export table-related APIs
         )
(require data/gvector
         racket/function
         racket/match
         racket/list
         "core.rkt"
         "ast.rkt"
         "type.rkt"
         "table.rkt"
         "union-find.rkt"
         "proofs.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; egraph

(struct egraph
  (;; Hash map from functioin to tables
   functions
   ;; Hash map from sorts to lists of constructors
   sorts
   ;;  Hash map from terms to lists of constructors
   terms
   ;; Hash map from term to sort and vice versa
   term->sort
   sort->term
   sort->@=>
   sort->=>@
   ;; Hash map from ruleset names to gvectors of
   ;; rule names
   rulesets
   ;; Hash map from rule names to rules
   rules
   ;; Hash map from symbols to E-class ids
   uf-mapper
   ;; #t if proof is generated
   proof?
   ))

(define (make-egraph #:proof? [proof? #f])
  (egraph
   ;; functions
   (make-hash (list (cons Impossible (make-table 'Impossible 0))))
   ;; sorts
   (make-hash)
   ;; terms
   (make-hash)
   ;; term->sort
   (make-hash)
   ;; sort->term
   (make-hash)
   ;; sort->@=>
   (make-hash)
   ;; sort->=>@
   (make-hash)
   ;; rulesets
   (make-hash (list (cons 'main (make-gvector))))
   ;; rules
   (make-hash)
   ;; uf-mapper
   (make-uf-mapper)
   ;; proof?
   proof?
   ))

(define (show-egraph egraph)
  (define function-asts
    (append*
     (for/list ([(fun table) (in-hash (egraph-functions egraph))])
       (cons (show-function fun)
             (show-table table)))))
  (define sort-asts
    (for/list ([sort (in-hash-keys (egraph-sorts egraph))])
      (show-base-type sort)))
  (append sort-asts function-asts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals
(define current-egraph (make-parameter (make-egraph)))
(define current-ruleset (make-parameter 'main))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;; Registers a function

(define (register-function egraph function)
  (define functions (egraph-functions egraph))
  (define arity (function-arity function))
  (define name (function-name function))
  (hash-set! functions function (make-table name arity)))

(define (register-constructor egraph function)
  (define otype (function-output-type function))
  (cond [(sort? otype) (let ([sorts (egraph-sorts egraph)])
                         (gvector-add! (hash-ref sorts otype) function))]
        [(term? otype) (let ([terms (egraph-terms egraph)])
                         (gvector-add! (hash-ref terms otype) function))]
        [else (error (format "~a is not a constructor" function))])

  (register-function egraph function))

;; Returns a table corresponding to the table
(define (lookup-function egraph function)
  (define functions (egraph-functions egraph))
  (hash-ref functions function))

;; Evaluates a funcion on the given input
;; Returns two values: the evaluated result
;; and whether a new tuple is created
;; in the database
(define (eval!-function egraph function args)
  (define uf-mapper (egraph-uf-mapper egraph))
  (define table (lookup-function egraph function))
  (define access-pattern (append-hole args))
  (define tuples (table-get table access-pattern))
  (if (null? tuples)
      (let* ([otype (function-output-type function)]
             [new-val (new-value! uf-mapper otype)]
             [new-tuple (append args (list new-val))])
        (table-append! table new-tuple)
        (values new-val #t))
      (values (only (only tuples)) #f)))

;; Runs the `set` action on the given function
;; Returns true if the database if updated
(define (set!-function egraph function args new-val)
  (define uf-mapper (egraph-uf-mapper egraph))
  (define table (lookup-function egraph function))
  (define access-pattern (append-hole args))
  (define old-vals (table-get table access-pattern #:full-tuple? #f))

  (unless (<= (length old-vals) 1)
    (raise "Functional dependency is violated"))

  (if (null? old-vals)
      ;; Tuple does not exist, insert into the database
      (let ([new-tuple (append args (list new-val))])
        (table-append! table new-tuple)
        #t)

      (let* ([old-val (only (only old-vals))]
             [otype (function-output-type function)]
             [val (merge-fn! uf-mapper otype (list old-val new-val))])
        (if (not (equal? old-val val))
            (let ([old-tuple (append args (list old-val))]
                  [new-tuple (append args (list val))])
              (table-remove! table old-tuple)
              (table-append! table new-tuple)
              #t)
            #f))
      ))

;; Registers a sort
(define (register-sort egraph sort)
  (define sorts (egraph-sorts egraph))
  (when (hash-has-key? sorts sort)
    (error (format "sort ~a already registered" sort)))
  (hash-set! sorts sort (make-gvector)))

;; Registers a term
(define (register-term egraph term)
  (define terms (egraph-terms egraph))
  (hash-set! terms term (make-gvector)))

(define (register-sort-term-pair sort term =>@ @=> (egraph (current-egraph)))
  (hash-set! (egraph-sort->term egraph) sort term)
  (hash-set! (egraph-term->sort egraph) term sort)
  (hash-set! (egraph-sort->=>@ egraph) sort =>@)
  (hash-set! (egraph-sort->@=> egraph) sort @=>))

(define (get-=>@ sort (egraph (current-egraph)))
  (hash-ref (egraph-sort->=>@ egraph) sort))

(define (get-@=> sort (egraph (current-egraph)))
  (hash-ref (egraph-sort->@=> egraph) sort))

(define (get-term-or-id maybe-sort (egraph (current-egraph)))
  (hash-ref (egraph-sort->term egraph) maybe-sort maybe-sort))

;; Registers a rule
(define (register-rule rule [egraph (current-egraph)] [ruleset (current-ruleset)])
  ;; Add rule to rules
  (define rules (egraph-rules egraph))
  (define name (rule-name rule))
  (when (hash-has-key? rules name)
    (error (format "~a is already registered" name)))
  (hash-set! rules name rule)

  ;; Register rules in the ruleset
  (define rulesets (egraph-rulesets egraph))
  (define rule-names (hash-ref! rulesets ruleset (thunk (make-gvector))))
  (gvector-add! rule-names name))

;; Runs an action
(define (run-action! action [cause 'user-action] [egraph (current-egraph)])
  (define as (actions (list action)))
  (define core-actions (flatten-actions as))
  (run-core-actions! egraph core-actions cause)
  (void))

(define (run-query query [egraph (current-egraph)])
  ;; TODO: For now, let's just rebuild the egraph manually
  ;; but what we really should do is to add a dirty bits to egraphs
  ;; and rebuild everytime needed
  (rebuild egraph)
  (define core-query (compile-query query egraph))
  (run-core-query egraph core-query))

;; Runs a ruleset until saturation
(define (saturate [ruleset (current-ruleset)] [egraph (current-egraph)])
  (let go ([acc-report (make-run-report)])
    (define new-report (run1 ruleset egraph))
    (define report (combine-report acc-report new-report))
    (if (zero? (run-report-tuple-update-count new-report))
        report
        (go report))))

;; Runs a ruleset for one iteration
(define (run1 [ruleset (current-ruleset)] [egraph (current-egraph)])
  (define rule-names (hash-ref (egraph-rulesets egraph) ruleset))
  ;; For each rule name, accumulate the rule and its compiled version
  (define-values (rules compiled-rules)
    (for/lists (rules compiled-rules)
               ([name (in-gvector rule-names)])
      (define rule (hash-ref (egraph-rules egraph) name))
      (values rule (compile rule egraph))))

  ;; search
  (define search-start-time (current-milliseconds))
  (define matches
    (map (curry run-core-query egraph)
         (map core-rule-query compiled-rules)))

  (for ([table (in-hash-values (egraph-functions egraph))])
    (drop-nonessential-indexes! table))

  ;; apply
  (define apply-start-time (current-milliseconds))
  (define apply-update-count
    (for/sum ([rule (in-list compiled-rules)]
              [ms (in-list matches)])
      (define actions (core-rule-actions rule))
      (unless (zero? (length ms))
        (displayln (format "Running ~a for ~a times" (core-rule-name rule) (length ms))))
      (for/sum ([m ms])
        ;; TODO: rule should contain original AST
        (define-values (m+ update-count) (run-core-actions! egraph actions rule m))
        update-count)))

  ;; rebuild
  (define rebuild-start-time (current-milliseconds))
  (define rebuild-update-count (rebuild egraph))
  (define rebuild-end-time (current-milliseconds))

  (run-report
   ;; search time
   (- apply-start-time search-start-time)
   ;; apply time
   (- rebuild-start-time apply-start-time)
   ;; rebuild time
   (- rebuild-end-time rebuild-start-time)
   ;; update count
   (+ apply-update-count rebuild-update-count)
   ;; iteration number
   1))

(define (run n [ruleset (current-ruleset)] [egraph (current-egraph)])

  (define initial (make-run-report))

  (define (do n result)
    (if (zero? n) result
        (let ([curr (run1 ruleset egraph)])
          (do (sub1 n) (combine-report curr result)))))

  (do n initial))

(define (run-core-query egraph query)
  (define (go egraph atoms m)
    (define (eval-arg arg) (if (symbol? arg)
                               (let ([val (assoc arg m)])
                                 (if val (cdr val)
                                     (pattern-hole)))
                               arg))
    (match atoms
      ['() (list m)]
      ;; core-check case
      [(cons (core-check arg) atoms+)
       (define val (eval-arg arg))
       (cond [(pattern-hole? val) (error (format "~a is not bound" val))]
             [(not val) '()]
             [else (go egraph atoms+ m)])]
      ;; core-atom case
      [(cons (core-atom fun args) atoms+)
       (let ([pat (map eval-arg args)])
         (cond [(function? fun)
                ; function case
                (define table (lookup-function egraph fun))
                (define result (table-get table pat #:full-tuple? #t))
                (define (bound-and-proceed tuple)
                  (define-values (m+ valid?)
                    ; `valid?` is used to handle non-linear patterns
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
                (when (ormap pattern-hole? in-pats)
                  (raise (format "Cannot execute query as ~a cannot be fully instantiated." (car atoms))))
                (define computed-val (apply (computed-function-run fun) in-pats))
                (cond [(pattern-hole? out-pat) (go egraph atoms+ (cons (cons (last args) computed-val) m))]
                      [(equal? out-pat computed-val) (go egraph atoms+ m)]
                      [else '()])]
               [else (raise (format "unsupported atom ~a" (car atoms)))]
               ))]
      ))

  (go egraph (core-query-atoms query) '()))

;; Returns the result context after running actions
;; as well as the number of updated tuples
(define (run-core-actions! egraph actions [cause #f] [m '()])
  ;; context will be updated at the end
  (define jus (justification cause #f))
  (define (eval-arg m arg)
    (if (symbol? arg)
        (cdr (assoc arg m))
        arg))
  (define-values (context updates)
    (let go ([actions (core-actions-actions actions)]
             ;; TODO termify converts e-classes in the context to corresponding
             ;; terms. Not yet sure how this will work out
             [m (termify egraph m)]
             ;; Number of updates to the database
             [updates 0])
      (match actions
        [(cons action rest)
         (match action
           [(core-let-atom-action var fun args)
            (define args+ (map (curry eval-arg m) args))
            (define-values (val updated?)
              (cond [(function? fun) (eval!-function egraph fun args+)]
                    ;; [(function? fun) (eval!-function egraph fun args+ jus)]
                    [(computed-function? fun) (values (apply (computed-function-run fun) args+) #f)]))
            (define m+ (cons (cons var val) m))
            (define updates+ (+ updates (if updated? 1 0)))
            (go rest m+ updates+)]
           [(core-let-val-action var val)
            (define val+ (eval-arg m val))
            (define m+ (cons (cons var val+) m))
            (go rest m+ updates)]
           [(core-set-action fun args expr)
            (define args+ (map (curry eval-arg m) args))
            (define expr+ (eval-arg m expr))
            ;; (set!-function egraph fun args+ expr+ jus)
            (define updated? (set!-function egraph fun args+ expr+))
            (define updates+ (+ updates (if updated? 1 0)))
            (go rest m updates+)]
           [(core-union-action v1 v2)
            ;; TODO: refactor this
            (define uf-mapper (egraph-uf-mapper egraph))
            (define v1+ (hash-ref uf-mapper (eval-arg m v1)))
            (define v2+ (hash-ref uf-mapper (eval-arg m v2)))
            (uf-union! v1+ v2+ jus)
            ;; We don't update counts for union, as
            ;; this will be taken care of during rebuilding
            (go rest m updates)])]
        ['() (values m updates)])))

  (set-justification-context! jus m)
  (values context updates))

(define (rebuild egraph)
  (define uf-mapper (egraph-uf-mapper egraph))
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
                          (define new-tuple (map (curry canonicalize uf-mapper) full-type tuple))
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
                                (define new-out-val (merge-fn! uf-mapper otype out-vals))
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

;; Utility function
(define (only lst) (match lst [(list e) e]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run report

(struct run-report
  (search-time
   apply-time
   rebuild-time
   tuple-update-count
   iter) #:transparent)

(define (make-run-report)
  (run-report 0 0 0 0 0))

(define (combine-report r1 r2)
  (run-report (+ (run-report-search-time r1) (run-report-search-time r2))
              (+ (run-report-apply-time r1) (run-report-apply-time r2))
              (+ (run-report-rebuild-time r1) (run-report-rebuild-time r2))
              (+ (run-report-tuple-update-count r1) (run-report-tuple-update-count r2))
              (+ (run-report-iter r1) (run-report-iter r2))))
