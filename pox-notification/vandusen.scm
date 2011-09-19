(module pox-notification/vandusen ()

(import chicken scheme)
(use tcp data-structures ports extras pox-notification)

(register-notification-handler 'vandusen
  (lambda (user params event new old)
    (unless (= user (alist-ref 'updater_id new))
      (receive (in out) 
	(tcp-connect (alist-ref 'host params) (alist-ref 'port params))
	(let ((message (conc (alist-ref 'recipient params) " " (format-event user event new old))))
	  (with-output-to-port out (cut print message)))
	(close-input-port in)
	(close-output-port out)))))

)