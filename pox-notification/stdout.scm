(module pox-notification/stdout ()

(import chicken scheme)
(use srfi-18 pox-notification extras)

(register-notification-handler 'stdout
  (lambda (user params changes)
    (print "=========== NOTIFICATION FOR " user " ===========")
    (pp (list params: params))
    (pp (list changes: changes))
    (print "===========")))

)