(module pox-notification

(register-notification-handler notification-ref format-event notification-initialize)

(import chicken scheme data-structures)
(use postgresql downtime pox-db pox-db/helpers)

(define notification-handlers '())

(define (register-notification-handler name handler)
  (set! notification-handlers (alist-update! name handler notification-handlers)))

(define (notification-initialize)
  (with-db-connection
   (lambda ()
     (for-each (lambda (handler)
		 (condition-case 
		  (unless (db-select-one 'notification_handlers 'name handler 'id)
		    (query (db-connection) "INSERT INTO notification_handlers (name) VALUES ($1)" handler))
		  (exn (postgresql) (unless (string=? "23505" (get-condition-property exn 'postgresql 'error-code))
				      (signal exn)))))
	       (map (compose symbol->string car) notification-handlers)))))

(define (format-event user-id event new old)
  (conc (alist-ref 'updater new)
	": "
	(task->item-line new user-id)))

(define (notification-ref name)
  (alist-ref name notification-handlers))

)