(module pox-mail 
  (send-mail send-mail-with)
  (import chicken scheme)
  (define send-mail-with (make-parameter #f))
  (define (send-mail . args) (apply (send-mail-with) args)))