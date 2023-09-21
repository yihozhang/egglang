#lang racket/base

(require "./lib.rkt")

;; sort
(sort S)

(function (add S S) S)
(function (var i64) S)

(require "egraph.rkt")
(require "macros.rkt")

(run-action! (make-action
              (add (var 0) (add (var 1)
                                (add (var 2)
                                     (add (var 3)
                                          (add (var 4)
                                               (add (var 5)
                                                    (add (var 6)
                                                         (add (var 7)
                                                              (add (var 8) (var 9))))))))))
              ))

(rule ([= e1 (add x y)])
      ([union e1 (add y x)]))

(rule ([= e1 (add x (add y z))])
      ([union e1 (add (add x y) z)]))

; (run 8)
(require profile)
; (require profile/render-graphviz)
(require racket/function)
(profile-thunk (thunk (run 8))
               ; #:render render
               )

(table-length (hash-ref (egraph-functions (current-egraph)) add))