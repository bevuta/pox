(module pox-notification/vandusen

()

(import chicken scheme)
(use tcp data-structures extras pox-notification)

(register-notification-handler 'vandusen
  (lambda (user params changes)
    (receive (in out)
        (tcp-connect (alist-ref 'host params) (alist-ref 'port params))
      (for-each (lambda (change)
                  (fprintf out "~A ~A~%"
                           (alist-ref 'recipient params)
                           (change->notification user change)))
                changes)
      (close-input-port in)
      (close-output-port out))))

)