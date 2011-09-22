(module pox-mail/smtp

(smtp-config smtp-send)

(import chicken scheme)
(use hato-smtp pox-log)

(define-logger log mail smtp)

(define smtp-config (make-parameter '()))

(define (smtp-send . args)
  (log (debug) (cons 'mail args))
  (apply send-mail (append (smtp-config) args)))

)