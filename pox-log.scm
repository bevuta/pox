(module pox-log

(log log-level log-filter)

(import chicken scheme)
(use data-structures extras vector-lib srfi-1)

(define log-levels 
  '#(debug: info: warning: critical:))

(define (log-level-index l)
  (or (vector-index (lambda (m) (eq? l m)) log-levels)
      (error 'log "invalid log level" l)))

(define log-level
  (make-parameter warning: log-level-index))

(define log-filter
  (make-parameter '()))

(define (log level . args)
  (when (and (<= (log-level) (log-level-index level)))
    (receive (keys msg)
        (span keyword? args)
      (when (and (not (< (length keys) (length (log-filter))))
                 (every (lambda (filter-key key)
                          (eq? filter-key key))
                        (log-filter)
                        keys))
        (printf "[~A] ~A: "
                (keyword->string level)
                (string-intersperse (map keyword->string keys)))
        (if (= 1 (length msg))
            (if (string? (car msg))
                (print (car msg))
                (begin
                  (when (list? (car msg))
                    (newline))
                  (pp (car msg))))
            (begin 
              (newline)
              (pp msg)))))))

)