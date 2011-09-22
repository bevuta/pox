(module pox-notification

(register-notification-handler notification-ref notification-initialize change->notification)

(import chicken scheme)
(use data-structures extras postgresql downtime pox-db pox-db/helpers pox-log)

(define-logger log notification)

(define notification-handlers '())

(define (register-notification-handler name handler)
  (set! notification-handlers (alist-update! name handler notification-handlers)))

(define (notification-initialize)
  (log (info) "initializing notification handlers")
  (with-db-connection
   (lambda ()
     (for-each (lambda (handler)
                 (log (debug) "notification handler: ~A" handler)
                 (condition-case
                     (unless (db-select-one 'notification_handlers 'name handler 'id)
                       (query (db-connection) "INSERT INTO notification_handlers (name) VALUES ($1)" handler))
                   (exn (postgresql) (unless (string=? "23505" (get-condition-property exn 'postgresql 'error-code))
                                       (signal exn)))))
               (map (compose symbol->string car) notification-handlers)))))

(define (change->notification user-id change)
  (sprintf "~A: ~A"
           (alist-ref 'updater (cadr change))
           (task->item-line (cadr change) user-id)))

(define (notification-ref name)
  (alist-ref name notification-handlers))

)