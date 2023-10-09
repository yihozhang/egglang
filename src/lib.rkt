#lang racket/base

(require "type.rkt")
(require "ast.rkt")
(require "core.rkt")
(require "macros.rkt")
(require "egraph.rkt")
(provide (rename-out [make-rule rule]
                     [make-sort sort]
                     [make-term term]
                     [make-function function]
                     [make-relation relation]
                     [make-ruleset ruleset]
                     [make-rewrite rewrite]
                     [make-datatype datatype]
                     [make-run-action! run-action!]
                     [make-check check]
                     [make-run-query run-query])
         declare-const define-const
         show-function show-rule
         (except-out (all-from-out "egraph.rkt") run-action! run-query)
         (all-from-out "core.rkt")
         (except-out (all-from-out "type.rkt") sort term function))
