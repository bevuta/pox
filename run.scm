#!/usr/bin/csi -ns

(use spiffy system)

(define mode (if (null? (command-line-arguments))
		 "development"
		 (car (command-line-arguments))))

(define development? (string=? mode "development"))

(if development? (access-log (current-output-port)))
(tcp-buffer-size 1024)

(define (with-system sys reload?)
  (let ((mutex   (make-mutex))
        (loaded? #f))
    (lambda (continue)
      (dynamic-wind
          (lambda ()
            (mutex-lock! mutex))
          (lambda ()
            (when (or reload? (not loaded?))
              (load-system sys)
              (set! loaded? #t))
            (continue))
          (lambda ()
            (mutex-unlock! mutex))))))

(load "pox.system")
(load-system pox)
(import pox-server spiffy-chain)
(root-path "server/static")
(vhost-map `((".*" . ,(chain-handlers (with-system pox development?) pox-handler))))

(print "listening on http://localhost:7040/")
(start-server port: 7040)