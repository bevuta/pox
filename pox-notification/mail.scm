(module pox-notification/mail

(notification-mail-from)

(import chicken scheme)
(use extras data-structures pox-notification pox-mail pox-model)

(define notification-mail-from (make-parameter #f))

(register-notification-handler 'mail
  (lambda (user params changes)
    (send-mail To:      (alist-ref 'recipient params)
               From:    (or (alist-ref 'from params) (notification-mail-from))
               Subject: (sprintf (or (alist-ref 'subject params) "[pox] changes by ~A")
                                 (alist-ref 'updater (cadar changes)))
               Body:    (task-list->string (map cadr changes) user))))

)