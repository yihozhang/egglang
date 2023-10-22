#lang racket

(require "../src/lib.rkt")


(datatype Math
          (Num i64)
          (Add Math Math)
          (Mul Math Math))

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

(run-action! (Add@ (Add@ (Num@ 1) (Num@ 2)) (Num@ 3)))

(for ([i 10])
  (run1)
  (run 10 '@))

(pretty-print (run-query (string-of (Add@ (Add@ (Num@ 1) (Num@ 2)) (Num@ 3)))))
