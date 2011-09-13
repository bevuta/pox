(module spiffy-auth

(register-authenticator! available-authenticators
 current-authentication current-credentials
 with-authentication)

(import chicken scheme)
(use srfi-1 data-structures)

(define-record authenticator
  get-credentials check-credentials handle-error)

(define authenticators '())

(define (register-authenticator! name get-credentials check-credentials handle-error)
  (set! authenticators 
        (alist-cons name
                    (make-authenticator get-credentials
                                        check-credentials
                                        handle-error) 
                    authenticators)))

(define available-authenticators
  (make-parameter '()))

(define current-authentication
  (make-parameter #f))

(define current-credentials
  (make-parameter #f))

(define (get-credentials)
  (if (null? (available-authenticators))
      (error 'with-authentication "No authenticators available")
      (let loop ((av-auth (available-authenticators)))
        (if (null? av-auth)
            (values #f #f)
            (let* ((auth-name (car av-auth))
                   (auth (alist-ref auth-name authenticators)))
              (if auth
                  (let ((credentials ((authenticator-get-credentials auth))))
                    (if credentials
                        (values auth credentials)
                        (loop (cdr av-auth))))
                  (error 'with-authentication "Unknown authenticator" auth-name)))))))

(define (with-authentication handler)
  (lambda (continue . rest)
    (receive (authenticator credentials)
        (get-credentials)
      (if authenticator
          (parameterize ((current-credentials credentials))
            (let ((auth ((authenticator-check-credentials authenticator))))
              (if auth
                  (parameterize ((current-authentication auth))
                    (apply handler continue rest))
                  ((authenticator-handle-error authenticator) continue))))
          ((authenticator-handle-error (alist-ref (car (available-authenticators)) authenticators))
           continue)))))

)