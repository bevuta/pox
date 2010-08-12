(module pox-server (pox-handler)

(import chicken scheme ports data-structures)
(use spiffy srfi-1 extras json intarweb spiffy-request-vars spiffy-uri-match pox-db/helpers pox-model downtime uri-common)

(include "web")

(define (list-string->list ls)
  (if ls (with-input-from-string (format "(~A)" ls) read) '()))

(define get-user-tasks
  (let ((handle (http-accept-case (current-request)
		  ((application/json) (json-response (lambda (user tasks omit-origin)
						       (map (cut list->vector <>) tasks))))
		  ((text/x-downtime)
		   (lambda (user tasks omit-origin)
		     (if (or (not tasks) (null? tasks))
			 (if (db-select-one 'users 'name user 'id)
			     (send-response code: 200 body: "")
			     (send-response code: 404 reason: "Not Found"))
			 (send-response headers: '((content-type #(text/x-downtime ((charset . "utf-8")))))
					body: (task-list->string tasks
								 user 
								 (and (not omit-origin)
								      (uri->string (server-root-uri)))))))))))

    (lambda (continue user)
      (parameterize ((user-map (select-users)))
	(let ((v (request-vars source: 'query-string))
	      (user (string->user-name user)))
	  (with-request-vars* v (group-by filter (include-done as-boolean) (omit-origin as-boolean))
			      (handle user
				      (select-user-tasks user 
							 (list-string->list group-by)
							 (list-string->list filter)
							 include-done)
				      omit-origin)))))))

(define (post-user-tasks continue user)
  (parameterize ((user-map (select-users)))
    (and-let* ((headers (request-headers (current-request)))
	       (content-length (header-value 'content-length headers))
	       (body (read-string content-length (request-port (current-request))))
	       (tasks (with-input-from-string body downtime-read))
	       (conflicts (persist-user-tasks (string->user-id user) tasks)))

      (if (null? conflicts)
	  (send-response code: 200 reason: "OK")
	  (send-response code: 409 reason: "Conflict" body: (conflicts->string conflicts (string->user-name user)))))))

(handle-exception
 (lambda (exn chain)
   (log-to (error-log) "~A" (build-error-message exn chain #t))
   (send-response code: 500 reason: (get-condition-property exn 'exn 'message))))

(define handle-request
  (uri-match/spiffy 
   `(((/ "inspect")
      (GET ,(lambda (continue) 
	      (send-response body: (with-output-to-string 
				       (cut pp (request-headers (current-request))))))))
     ((/ "users") 
      ((/ (submatch (+ any)))
       ((/ "tasks")
	(GET ,get-user-tasks)
	(POST ,post-user-tasks)))))))

(define (pox-handler continue)
  (with-db-connection (lambda ()
			(handle-request continue))))

)