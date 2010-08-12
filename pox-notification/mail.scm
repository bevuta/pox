(module pox-notification/mail

(notification-mail-from)

(import chicken scheme data-structures)
(use pox-notification pox-mail)

(define notification-mail-from (make-parameter #f))

(register-notification-handler 'mail
  (lambda (user params event new old)
    (unless (= user (alist-ref 'updater_id new))
      (send-mail To:	  (alist-ref 'recipient params)
		 From:	  (or (alist-ref 'from params) (notification-mail-from))
		 Subject: (format-event user event new old)
		 Body:	  (alist-ref 'description new)))))

)