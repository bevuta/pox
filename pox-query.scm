(module pox-query

(as-grouping as-filter)

(import chicken scheme)
(use extras data-structures ports srfi-1 srfi-13 sexpressive)

(define filter-syntax
  (append (syntax:whitespace)
          (syntax:symbols as-string: #t)))

(define (read-filter in)
  (parameterize ((sexpressive filter-syntax))
    (map (lambda (x)
           (if (eq? #\: (string-ref x 0))
               (string->keyword (string-drop x 1))
               x))
         (remove (lambda (x)
                   (and (string? x)
                        (< (string-length x) 2)))
                 (read-file in read*))))) 

(define (string->filter str)
  (call-with-input-string str read-filter))

(define grouping-syntax
  (append (syntax:whitespace)
          (syntax:symbols)))

(define (read-grouping in)
  (parameterize ((sexpressive grouping-syntax))
    (read-file in read*)))

(define (string->grouping str)
  (call-with-input-string str read-grouping))

(define (make-request-var-converter convert #!optional default)
  (lambda (variable params)
    (let ((val (alist-ref variable params)))
      (if val (convert val) default))))

(define as-grouping
  (make-request-var-converter string->grouping '()))

(define as-filter
  (make-request-var-converter string->filter '()))

)