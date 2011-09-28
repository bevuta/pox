(define mode (if (null? (command-line-arguments))
                 "development"
                 (car (command-line-arguments))))

(define development? (string=? mode "development"))

(cond-expand
 (compiling)
 (else
  (use system)
  (load "pox.system")
  (load-system pox)))

(use chicken-syntax uri-common spiffy spiffy-chain spiffy-system pox-server)

(server-port 7040)

(load "init")
(include "boot")

(when development?
  (access-log (current-output-port)))

(tcp-buffer-size 1024)
(root-path "server/static")

(define handler
  (cond-expand
   (compiling
    (make-pox-handler))
   (else
    (chain (with-system pox development?)
           (make-pox-handler)))))

(vhost-map `((".*" . ,handler)))

(define-logger log server)

(log (info)
     `((state . listening)
       (uri ,(uri->string (update-uri (uri-reference "")
                                      scheme: (if (secure-connection?) 'https 'http)
                                      port: (server-port)
                                      host: (or (server-bind-address)
                                                "0.0.0.0"))))))
(start-server port: 7040)