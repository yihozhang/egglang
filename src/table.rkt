#lang racket/base

(require racket/match
         racket/list
         racket/set
         racket/function
         data/bit-vector
         data/gvector)

(provide
 ;; Signature
 pattern->sig apply-sig
 ;; Table
 (struct-out table) show-table
 make-table table-get table-remove! table-append!
 table-length table->list
 drop-nonessential-indexes!)

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
;; Table

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
