(module pox-notification/stdout ()

(import chicken scheme)
(use srfi-18 pox-notification extras)

(register-notification-handler 'stdout 
  (lambda (user params event new old)
    (print "===========")
    (pp params)
    (pp event)
    (pp new)
    (pp old)
    (print "===========")))

)