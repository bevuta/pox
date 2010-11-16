(when development?
  (load "downtime")
  (load "pox-server")
  (load "pox-model")
  (use scss)
  (with-output-to-file "server/static/layout.css"
    (lambda ()
      (scss->css (eval (with-input-from-file "server/layout.scss" read))))))

(load "init")
(use (only pox-notification notification-initialize))
(notification-initialize)