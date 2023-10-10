#lang racket

(require "../src/lib.rkt")

(sort Math)
(term Math@)

(function (Num i64) Math)
(function (Num@ i64) Math@)

(function (Add Math Math) Math)
(function (Add@ Math@ Math@) Math@)

(function (Mul Math Math) Math)
(function (Mul@ Math@ Math@) Math@)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-of

(function (string-of Math@) String)

(rule ((= e@ (Num@ i)))
      ((displayln i)
       (displayln e@)
       (set! (string-of e@) (format "(Num ~a)" i))))

(rule ((= e@ (Add@ x@ y@))
       (= sx (string-of x@))
       (= sy (string-of y@)))
      ((set! (string-of e@) (string-append "(Add " sx " " sy ")"))))

(rule ((= e@ (Mul@ x@ y@))
       (= sx (string-of x@))
       (= sy (string-of y@)))
      ((set! (string-of e@) (string-append "(Mul " sx " " sy ")"))))

(function (from-term Math@) Math)

(rule ((= e@ (Num@ x)))
      ((set! (from-term e@) (Num x))))

(rule ((= e@ (Add@ x@ y@))
       (= x (from-term x@))
       (= y (from-term y@)))
      ((set! (from-term e@) (Add x y))))

(rule ((= e@ (Mul@ x@ y@))
       (= x (from-term x@))
       (= y (from-term y@)))
      ((set! (from-term e@) (Mul x y))))

(function (to-term Math) Math@)

(rule ((= (from-term e@) e))
      ((set! (to-term e) e@)))

(datatype proof
          (proof-edge Math@ Math@))

(relation (proof-edge-str String String))

(rule ((= r1 (Add (Add x y) z))
       (= x@ (to-term x))
       (= y@ (to-term y))
       (= z@ (to-term z)))
      ((let u (Add y z))
       (let r2 (Add x u))
       (union! r1 r2)
       (proof-edge (Add@ (Add@ x@ y@) z@)
                   (Add@ x@ (Add@ y@ z@)))))

(rule ((= r1 (Add x y))
       (= x@ (to-term x))
       (= y@ (to-term y)))
      ((let r2 (Add y x))
       (union! r1 r2)
       (proof-edge (Add@ x@ y@) (Add@ y@ x@))))

(rule ((proof-edge a@ b@)
       (= as (string-of a@))
       (= bs (string-of b@)))
      ((proof-edge-str as bs)))

(run-action! (Add@ (Add@ (Num@ 1) (Num@ 2)) (Num@ 3)))

(run 10)

(print-table proof-edge)
(print-table proof-edge-str)