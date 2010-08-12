(module pox-mail/sendmail

(sendmail-program sendmail-default-headers sendmail-send)

(import chicken scheme posix)
(use hato-smtp)

(define sendmail-program (make-parameter "/usr/bin/sendmail -t"))

(define sendmail-default-headers 
  (make-parameter '(Content-Type: "text/plain; charset=utf-8")))

(define (sendmail-send . args)
  (receive (in out pid)
    (process (sendmail-program))
    (apply smtp-write-message (cons out (append args (sendmail-default-headers))))
    (close-output-port out)))

)