(module pox-query

(as-grouping as-filter as-ignore)

(import chicken scheme)
(use extras data-structures ports srfi-1 srfi-13 sexpressive)

(define-syntax define-converter
  (syntax-rules ()
    ((_ name syntax fn default)
     (begin
       (define name #f)
       (let* ((syntax* syntax)
              (convert (lambda (str)
                         (parameterize ((sexpressive syntax*))
                           (call-with-input-string str
                             (lambda (in)
                               (reverse (port-fold fn '() (lambda () (read* in))))))))))
         (set! name
               (lambda (variable params)
                 (let ((val (alist-ref variable params)))
                   (if val (convert val) default)))))))
    ((_ name syntax fn)
     (define-converter name syntax fn '()))
    ((_ name syntax)
     (define-converter name syntax cons))))

(define-converter as-filter
  (append (syntax:whitespace)
          (syntax:symbols as-string: #t))
  (lambda (x filter)
    (if (and (string? x) (< (string-length x) 2))
        filter
        (cons (if (eq? #\: (string-ref x 0))
                  (string->keyword (string-drop x 1))
                  x)
              filter))))

(define-converter as-grouping
  (append (syntax:whitespace)
          (syntax:symbols)))

(define-converter as-ignore
  (append (syntax:whitespace)
          (syntax:symbols)))

)