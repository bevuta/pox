(module pox-notification/stdout ()

(import chicken scheme pox-notification extras)
(use srfi-18)

(register-notification-handler 'stdout 
  (lambda (user params event new old)
    (print "===========")
    (pp params)
    (pp event)
    (pp new)
    (pp old)
    (print "===========")))

)