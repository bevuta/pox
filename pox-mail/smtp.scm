(module pox-mail/smtp

(smtp-auth smtp-send)

(import chicken scheme)
(use hato-smtp)

(define smtp-auth (make-parameter '()))

(define (smtp-send . args)
  (apply send-mail (append (smtp-auth) args)))

)