(module pox-auth 

(valid-credentials? check-auth-credentials)

(import chicken scheme)
(use data-structures intarweb spiffy spiffy-session spiffy-auth pox-log)

(define check-auth-credentials
  (make-parameter (lambda _ (error "check-auth-credentials is not set"))))

(define (valid-credentials? user password)
  ((check-auth-credentials) user password))

(register-authenticator! 'session
                         (lambda ()
                           (current-session))
                         (lambda ()
                           (session-ref 'user))
                         (lambda (continue)
                           (send-response status: 'unauthorized
                                          body: "Invalid session")))

(available-authenticators '(session))

)