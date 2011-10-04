(module pox-query

(as-grouping as-filter as-list-of-symbols)

(import chicken scheme)
(use extras data-structures ports srfi-1 srfi-13 srfi-14 sexpressive)

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

(define (read-id port)
  (string->number
   (list->string
    (let loop ()
      (let ((char (peek-char port)))
        (cond ((eof-object?  char)
               '())
              ((char-set-contains? char-set:whitespace char)
               '())
              ((char-set-contains? char-set:digit char)
               (cons (read-char port) (loop)))
              (else 'read* "invalid char in id" char)))))))

(define-converter as-filter
  (append (syntax:whitespace)
          (syntax:keywords)
          `((#\# . ,(lambda (port)
                      (read-char port)
                      (read-id port))))
          (syntax:symbols as-string: #t))
  (lambda (x filter)
    (if (or (not x) (and (string? x) (< (string-length x) 2)))
        filter
        (cons x filter))))

(define-converter as-grouping
  (append (syntax:whitespace)
          (syntax:symbols)))

(define-converter as-list-of-symbols
  (append (syntax:whitespace)
          (syntax:symbols)))

)