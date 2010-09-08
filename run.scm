#!/usr/bin/csi -ns

(use spiffy)

(define mode (if (null? (command-line-arguments))
		 "development"
		 (car (command-line-arguments))))

(define development? (string=? mode "development"))

(if development? (access-log (current-output-port)))
(tcp-buffer-size 1024)

(define load-server
  (let ((loaded #f)
	(mutex  (make-mutex)))
    (lambda ()
      (when (or development? (not loaded))
	(mutex-lock! mutex)
	(load-relative "boot")
	(set! loaded #t)
	(mutex-unlock! mutex)))))

(load-server)
(use pox-server)
(root-path "server/static")
(vhost-map `((".*" . ,(lambda (continue) 
			(load-server) 
			(pox-handler continue)))))

(print "listening on http://localhost:7040/")
(start-server port: 7040)