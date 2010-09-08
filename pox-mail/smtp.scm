(module pox-mail/smtp

(smtp-config smtp-send)

(import chicken scheme)
(use hato-smtp)

(define smtp-config (make-parameter '()))

(define (smtp-send . args)
  (apply send-mail (append (smtp-config) args)))

)