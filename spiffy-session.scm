(module spiffy-session

(session-cookie-name current-session with-session 
 get-session-id get-session-id-from-cookie
 on-session-start on-session-refresh with-new-session
 session-lifetime session-ref session-set! session-del!)

(import chicken scheme)
(use data-structures srfi-1 intarweb spiffy (prefix http-session http-))

(define current-session
  (make-parameter #f))

(define session-cookie-name
  (make-parameter "spiffy-session"))

(define (get-session-id-from-cookie)
  (and-let* ((cookies (header-values 'cookie (request-headers (current-request))))
             (cookie (find (lambda (cookie)
                             (and (equal? (session-cookie-name) (car cookie))
                                  (http-session-valid? (cdr cookie))))
                           cookies)))
    (cdr cookie)))

(define get-session-id
  (make-parameter get-session-id-from-cookie))

(define (set-session-cookie!)
  (let* ((headers (response-headers (current-response)))
         (headers (update-header-contents! 'set-cookie
                                           `(#((,(session-cookie-name) . ,(current-session))
                                               ((max-age . ,(session-lifetime)))))
                                           headers))
         (response (update-response (current-response) headers: headers)))
    (current-response response)))

(define on-session-start
  (make-parameter set-session-cookie!))

(define on-session-refresh
  (make-parameter set-session-cookie!))

(define (with-new-session continue)
  (when (current-session)
    (http-session-destroy! (current-session)))
  (parameterize ((current-session (http-session-create)))
    ((on-session-start))
    (continue)))

(define (with-session handler #!key (auto-refresh #t) (auto-start #f))
  (lambda (continue)
    (let ((sid ((get-session-id))))
      (parameterize ((current-session (and (http-session-valid? sid) sid)))
        (when auto-refresh (session-refresh!))
        (if (and auto-start (not (current-session)))
            (with-new-session (lambda () (handler continue)))
            (handler continue))))))

;; basically http-session re-exports

(define session-lifetime http-session-lifetime)

(define (session-ref var #!optional default)
  (http-session-ref (current-session) var default))

(define (session-set! var val)
  (http-session-set! (current-session) var val))

(define (session-del! var)
  (http-session-del! (current-session) var))

(define (session-refresh!)
  (when (current-session)
    (http-session-refresh! (current-session))
    ((on-session-refresh))))

)