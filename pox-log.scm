(module pox-log

(log-for define-logger log-target)

(import chicken scheme)
(use extras srfi-69 log5scm)

(reexport (rename log5scm (define-category define-log-category)))

(begin-for-syntax
 (import chicken)
 (use srfi-1))

(define-category debug)
(define-category info)
(define-category warning)
(define-category critical)

(define-category all
  (or debug info warning critical))

(define (log-target port-or-path #!optional (cat-spec 'all) (name 'default))
  (add-sender (make-sender name: name 
                           handler: (port-sender port-or-path)
                           output-spec: '(message) ; set in *default-output-spec* once #698 is fixed
                           category-spec: (if (list? cat-spec)
                                              cat-spec
                                              (list cat-spec)))))

(define-syntax define-logger
  (syntax-rules ()
    ((_ log component ...)
     (begin
       (use (only ports with-output-to-string)
            (only srfi-1 cons*)
            (only extras pp sprintf))
       (define-log-category component) ...
       (define-syntax log
         (ir-macro-transformer
          (lambda (x i c)
            (if (null? (second x))
                (error 'log "log category is mandatory")
                (let* ((categories `(component ... . ,(second x)))
                       (body (cddr x)))
                  `(find-and-apply-senders
                    ',categories
                    "~A"
                    (with-output-to-string
                        (lambda ()
                          (pp (let* ((msg ,(car body)) ; make sure msg is only evaluated once
                                     (body (cons msg (list . ,(cdr body)))))
                                (cons* (cons 'categories ',categories)
                                       (cons 'time (current-seconds))
                                       (if (string? msg)
                                           (list (cons 'message (apply sprintf body)))
                                           body))))))))))))))))

)