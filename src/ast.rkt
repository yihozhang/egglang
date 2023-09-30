#lang racket/base

(require racket/string
         racket/match
         "type.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Surface IR declaration

(struct rule (query actions) #:transparent)
(struct query (atoms) #:transparent)
(struct actions (actions) #:transparent)

(struct value-eq (lhs rhs) #:transparent)
(struct call (fun args) #:transparent)

(struct let-action (var expr) #:transparent)
(struct set-action (fun args expr) #:transparent)
(struct union-action (e1 e2) #:transparent)

(define (show-rule rule)
  (define (show-expr expr)
    (cond
      [(call? expr)
       (define name (head-name (call-fun expr)))
       (define args (call-args expr))
       `(,name ,@(map show-expr args))]
      [#t expr]))

  (define (show-query query)
    (define atoms (query-atoms query))
    (define (show-atom atom)
      (match atom
        [(value-eq lhs rhs) `(= ,(show-atom lhs) ,(show-atom rhs))]
        [e (show-expr e)]))
    (map show-atom atoms))

  (define (show-actions actions)
    (define actions+ (actions-actions actions))
    (define (show-action action)
      (match action
        [(let-action var expr) `(let ,var ,(show-expr expr))]
        [(set-action fun args expr) `(set! (,(function-name fun) ,@(map show-expr args))
                                           ,(show-expr expr))]
        [(union-action e1 e2) `(union ,(show-expr e1) ,(show-expr e2))]
        [_ (show-expr action)]
        ))
    (map show-action actions+))

  `(rule ,(show-query (rule-query rule)) ,(show-actions (rule-actions rule))))
