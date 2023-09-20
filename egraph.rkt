#lang racket/base

(provide current-egraph current-ruleset
         register-function register-sort register-rule
         egraph-functions egraph-sorts egraph-rulesets
         run1)
(require data/gvector
         data/bit-vector
         racket/set
         data/union-find ;; one day we are going to use our own
         racket/function
         racket/match
         racket/list
         "core.rkt"
         "ast.rkt"
         "type.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; egraph

(struct egraph
  (functions
   sorts
   rulesets))

(define (make-egraph)
  (egraph
   (make-hash)
   (make-gvector)
   (make-hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;; Registers a function
(define (register-function egraph function)
  (define functions (egraph-functions egraph))
  (define arity (function-arity function))
  (hash-set! functions function (make-table arity)))

;; Returns a table corresponding to the table
(define (lookup-function egraph function)
  (define functions (egraph-functions egraph))
  (hash-ref functions function))

;; Evaluates a funcion on the given input
(define (eval!-function egraph function args)
  (define table (lookup-function egraph function))
  (define access-pattern (append args '(#f)))
  (define tuples (table-get table access-pattern))
  (if (null? tuples)
      (let* ([otype (function-output-type function)]
             [new-val (new-value! otype)]
             [new-tuple (append args (list new-val))])
        (table-append! table new-tuple)
        new-val)
      (only (only tuples))))

(define (set!-function egraph function args new-val)
  (define table (lookup-function egraph function))
  (define access-pattern (append args '(#f)))
  (define old-vals (table-get table access-pattern #f))
  (unless (<= (length old-vals) 1)
    (raise "Functional dependency is violated"))

  ;; figure out merge value
  (define val
    (if (not (null? old-vals))
        (let ([old-val (only old-vals)]
              [otype (function-output-type function)])
          (merge-fn! otype (list old-val new-val)))
        new-val))

  ;; remove the old tuple if exists
  (when (not null? old-vals)
    (define old-tuple (append args (list (only old-vals))))
    (table-remove! table old-tuple))

  ;; insert the new tuple
  (define new-tuple (append args (list val)))
  (table-append! table new-tuple))

(define (register-sort egraph sort)
  (define sorts (egraph-sorts egraph))
  (gvector-add! sorts sort))

(define (register-rule egraph rule #:ruleset ruleset)
  (define rulesets (egraph-rulesets egraph))
  (define rules (hash-ref! rulesets ruleset (thunk (make-gvector))))
  (gvector-add! rules rule))

(define current-egraph (make-parameter (make-egraph)))

(define current-ruleset (make-parameter 'main))

(define (run1 egraph ruleset)
  (define rules (hash-ref (egraph-rulesets egraph) ruleset))
  (define compiled-rules (for/list ([rule (in-gvector rules)]) (compile rule egraph)))

  ;; search
  (define matches (map (curry run-query egraph)
                       (map core-rule-query compiled-rules)))
  ;; apply
  (for ([rule (in-list compiled-rules)]
        [ms (in-list matches)])
    (define actions (core-rule-actions rule))
    (for ([m ms])
      (run-core-actions! egraph actions m)))
  (rebuild egraph))

(define (run-query egraph query) '(()))
(define (run-core-actions! egraph actions [m '()])
  (define (eval-arg m arg)
    (if (symbol? arg) (cdr (assoc arg m)) arg))
  (let go ([actions (core-actions-actions actions)]
           [m m])
    (match-define (cons action rest) actions)
    (match action
      [(core-let-atom-action var fun args)
       (define args+ (map (curry eval-arg m) args))
       (define val (eval!-function egraph fun args+))
       (define m+ (cons (cons var val) m))
       (go rest m+)]
      [(core-let-val-action var val)
       (define val+ (eval-arg m val))
       (define m+ (cons (cons var val+) m))
       (go rest m+)]
      [(core-set-action fun args expr)
       (define args+ (map (curry eval-arg m) args))
       (define expr+ (eval-arg m expr))
       (set!-function egraph fun args+ expr+)]
      [(core-union-action v1 v2)
       (uf-union! (eval-arg m v1) (eval-arg m v2))])))

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
                    (lambda (table)
                      (define tab-list (table->list table))
                      (define canon-tab-list
                        (for/list ([tuple tab-list])
                          (map canonicalize full-type tuple)))
                      (define grouped-by (group-by (lambda (tuple) (drop-right tuple 1)) canon-tab-list))

                      (define new-table (make-table arity))
                      (define count
                        (for*/sum ([group (in-list grouped-by)]
                                   [len (in-value (length group))]
                                   #:when (> len 1))
                          (define out-vals (map last group))
                          (define new-out-val (merge-fn! out-vals))
                          (define tuple (list-set (first group) (sub1 arity) new-out-val))
                          (table-append! new-table tuple)
                          (sub1 len)))
                      (set! update-count (+ update-count count))

                      new-table))
      update-count))
  (let rebuild ([count 0])
    (define new (rebuild-once))
    (if (zero? new)
        count
        (rebuild (+ count new)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access patterns and signatures

;; A signature is a bitvector mask
;; where #t means it's part of the index
;; A pattern is a signature converted to list
;; with every #t replaced with an actual value

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
  (arity
   buffer
   indexes))

(define (make-table arity)
  (define full-sig (make-bit-vector arity #t))
  (table arity
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
  (for/list ([id ids])
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
    (index-remove! index tuple)))

(define (table->list table)
  (define index (get-lookup-index table))
  (index-keys index))

;; Below are internal functions to table

(define (get-id-from-tuple table tuple)
  (define lookup-index (get-lookup-index table))
  (define ids (index-ref lookup-index tuple))
  (when (> (set-count ids) 1) (raise "Set semantics is violated"))
  (set-first ids))

(define (get-lookup-index table)
  (define arity (table-arity table))
  (define full-sig (make-bit-vector arity #t))
  ;; such an index should always exist
  (get-index! table full-sig))

(define (get-index! table sig)
  (define indexes (table-indexes table))
  (define (create-index)
    (define lookup-index (get-lookup-index table))
    (define hash (make-hash))
    (for* ([(tuple ids) (in-index lookup-index)]
           #:do [(when (> (length ids) 1)
                   (raise "Set semantics is violated"))]
           [id ids])
      (hash-set! hash (apply-sig sig tuple) id))
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
(define (index-ref index access-pattern [default (set)])
  (hash-ref (index-hash index) access-pattern default))

(define (index-add! index tuple id)
  (define hash (index-hash index))
  (define sig (index-sig index))
  (define pat (apply-sig sig tuple))
  (define ids (hash-ref! hash pat (thunk (mutable-set))))
  (set-add! ids id))

(define (index-remove! index tuple)
  (define hash (index-hash index))
  (define sig (index-sig index))
  (define pat (apply-sig sig tuple))
  (hash-remove! hash pat))

(define (index-keys index)
  (define hash (index-hash index))
  (hash-keys hash))

(define in-index (compose in-hash index-hash))