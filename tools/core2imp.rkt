#lang racket

(require "common.rkt" "fpcore.rkt" "fpimp.rkt")
(provide compile-program)

(define *names* (make-parameter (mutable-set)))

(define (gensym name)
  (define prefixed
    (filter (λ (x) (string-prefix? (~a x) (~a name))) (set->list (*names*))))
  (define options
    (cons name (for/list ([_ prefixed] [i (in-naturals)]) (string->symbol (format "~a_~a" name (+ i 1))))))
  (define name*
    (car (set-subtract options prefixed)))
  (set-add! (*names*) name*)
  name*)

(define (create-output-list)
  (let ([lst '()])
    (values
      (lambda (x) (set! lst (cons x lst)))
      (lambda () (reverse lst)))))

(define/contract (expr->imp out! expr names)
  (-> (-> statement? any) expr? (listof (cons/c symbol? symbol?)) expr?)
  ;; Takes an FPCore expression,
  ;; returns an FPImp expression, statements go to out!
  (match expr
    [`(let ([,vars ,vals] ...) ,body)
     (define vars* (map gensym vars))
     (for ([var* vars*] [val vals])
       (define val*
         (expr->imp out! val names))
       (out! `[= ,var* ,val*]))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (expr->imp out! body names*)]
    [`(if ,cond ,ift ,iff)
     (define cond* (expr->imp out! cond names))
     (define outvar (gensym 'temp))
     (define-values (ift-out! ift-get) (create-output-list))
     (define ift* (expr->imp ift-out! ift names))
     (define-values (iff-out! iff-get) (create-output-list))
     (define iff* (expr->imp iff-out! iff names))
     (out! `(if [,cond* ,@(ift-get) [= ,outvar ,ift*]]
                [else ,@(iff-get) [= ,outvar ,iff*]]))
     outvar]
    [`(while ,cond ([,vars ,inits ,updates] ...) ,retexpr)
     (define vars* (map gensym vars))
     (for ([var* vars*] [val inits])
       (define val*
         (expr->imp out! val names))
       (out! `[= ,var* ,val*]))
     (define names*
       (for/fold ([names* names]) ([var vars] [var* vars*])
         (dict-set names* var var*)))
     (define test-var (gensym 'test))
     (define cond*
       (expr->imp out! cond names*))
     (out! `[= ,test-var ,cond*])
     (define-values (body-out! body-get) (create-output-list))
     (define temp-vars (map gensym vars))
     (for ([temp-var temp-vars] [update updates])
       (define update*
         (expr->imp body-out! update names*))
       (body-out! `[= ,temp-var ,update*]))
     (for ([var* vars*] [temp-var temp-vars])
       (body-out! `[= ,var* ,temp-var]))
     (define recond*
       (expr->imp body-out! cond names*))
     (body-out! `[= ,test-var ,recond*])
     (out! `(while ,test-var ,@(body-get)))
     (expr->imp out! retexpr names*)]
    [`(,(? operator? operator) ,args ...)
     (define args*
       (map (λ (arg) (expr->imp out! arg names)) args))
     `(,operator ,args*)]
    [(? constant?)
     expr]
    [(? symbol?)
     (dict-ref names expr)]
    [(? number?)
     expr]))

(define/contract (compile-program prog)
  (-> fpcore? fpimp?)
  (match-define (list 'FPCore (list args ...) props ... body) prog)
  (define-values (body-out! body-get) (create-output-list))
  (define names
    (map (lambda (x) (cons x x)) args))
  (define body*
    (expr->imp body-out! body names))
  `(FPImp (,@args) ,@props ,@(body-get) (output ,body*)))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "core2imp.rkt"
   #:args ()
   (port-count-lines! (current-input-port))
   (for ([expr (in-port (curry read-fpcore "stdin"))] [n (in-naturals)])
     (pretty-print (compile-program expr)))))
