(when development?
  (load "downtime")
  (load "pox-server")
  (load "pox-model"))

(load "init")
(use (only pox-notification notification-initialize))
(notification-initialize)