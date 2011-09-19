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

(use chicken-syntax spiffy spiffy-chain spiffy-system pox-server)

(load "init")
(include "boot")

(when development?
  (access-log (current-output-port)))

(tcp-buffer-size 1024)
(root-path "server/static")

(define handler
  (cond-expand
   (compiling
    pox-handler)
   (else
    (use system)
    (load "pox.system")
    (load-system pox)
    (chain (with-system pox development?) pox-handler))))

(vhost-map `((".*" . ,handler)))

(print "listening on http://localhost:7040/")
(start-server port: 7040)