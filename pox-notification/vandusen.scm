(module pox-notification/vandusen

(vandusen-config)

(import chicken scheme)
(use tcp data-structures extras pox-notification)

(define vandusen-config
  (make-parameter '()))

(define (config-ref setting params)
  (or (alist-ref setting params) (alist-ref setting (vandusen-config))))

(register-notification-handler 'vandusen
  (lambda (user params changes)
    (receive (in out)
        (tcp-connect (config-ref 'host params) (config-ref 'port params))
      (for-each (lambda (change)
                  (fprintf out "~A ~A~%"
                           (alist-ref 'recipient params)
                           (change->notification user change)))
                changes)
      (flush-output out)
      (close-input-port in)
      (close-output-port out))))

)