#lang racket/base

(require racket/match
         "egraph.rkt"
         "type.rkt"
         "ast.rkt")

(provide make-@-rules)

(define (make-@-rules D D:@=> D:=>@ constructors)
  ;; conversion rules from datatype to datatype@ (terms)
  (define @=>-rules
    (for/list ([constructor-sig (in-list constructors)])
      (define func (car constructor-sig))
      (define func+ (cadr constructor-sig))
      (define args (cddr constructor-sig))

      (define sort-vars
        (for/list ([_ args]
                   [i (in-naturals)])
          (string->symbol (format "x~a" i))))
      (define term-vars
        (for/list ([_ args]
                   [i (in-naturals)])
          (string->symbol (format "x~a@" i))))

      (define top-atom (value-eq 'e@ (call func+ term-vars)))
      (define mapping-atoms
        (for/list ([arg args]
                   [term-var term-vars]
                   [sort-var sort-vars])
          (if (sort? arg)
              (let* ([arg:@=> (get-@=> arg)])
                (value-eq sort-var (call arg:@=> (list term-var))))
              (value-eq sort-var term-var))))

      (define q (query (cons top-atom mapping-atoms)))
      (define as (actions (list (set-action D:@=> '(e@) (call func sort-vars)))))

      (define name (string->symbol (format "~a:@=>--~a"
                                           (base-type-name D)
                                           (function-name func+))))
      (rule name q as)))

  ;; conversion rules from datatype@ (terms) to datatype
  (define =>@-rule
    (let ()
      ;; (rule ((= e (D:@=> e@)))
      ;;       ((set (D:=>@ e) e@))
      (define name (string->symbol (format "~a:=>@" (base-type-name D))))
      (define q (query (list (value-eq 'e (call D:@=> '(e@))))))
      (define as (actions (list (set-action D:=>@ '(e) 'e@))))
      (rule name q as)))

  (parameterize ([current-ruleset '@])
    (for ([rule (in-list @=>-rules)])
      (register-rule rule))
    (register-rule =>@-rule)))
