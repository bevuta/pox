(module pox-server (pox-handler)

(import chicken scheme)
(use spiffy srfi-1 extras ports data-structures medea intarweb spiffy-request-vars
     spiffy-uri-match pox-db/helpers pox-model downtime uri-common sxml-transforms
     irregex)

(include "web")

(define (list-string->list ls)
  (if ls (with-input-from-string (format "(~A)" ls) read) '()))

(define (send-page id title content nav)
  (send-sxml-response
   `((head (title ,title))
     (body
      (div (@ (data-role "page") (id ,id))
           (div (@ (data-role "header") (class "ui-bar")) 
                ,nav
                (h1 ,title))
           (div (@ (data-role "content")) ,content))))))

(define (send-tasks-response select-tasks #!optional user)
  (parameterize ((user-map (select-users)))
    (with-request-vars* (request-vars source: 'query-string)
        (group-by filter (include-done as-boolean) (omit-origin as-boolean))
      (let ((tasks (select-tasks (list-string->list group-by)
                                 (list-string->list filter)
                                 include-done)))
        (if (not tasks)
            (send-response code: 404 reason: "Not Found" body: "Not Found")
            (http-accept-case (current-request)
              ((application/json) 
               (send-json-response (task-list->json-serializable tasks)))
              ((text/x-downtime)
               (send-response headers: '((content-type #(text/x-downtime ((charset . "utf-8")))))
                              body: (task-list->string 
                                     tasks
                                     user 
                                     (and (not omit-origin)
                                          (request-uri (current-request))))))
              ((text/html)
               (send-page "tasks"
                          (if user
                              `("Tasks of " ,user)
                              "Tasks")
                          `(div (@ (id "tasks") (data-role "collapsible-set"))
                                ,(map (lambda (t)
                                        `(div (@ (data-role "collapsible"))
                                              (h3 ,(alist-ref 'name t))
                                              ,(and-let* ((desc (alist-ref 'description t)))
                                                 (simple-format desc))
                                              (div (@ (data-role "navbar"))
                                                   (ul (li (a (@ (href "/tasks/" ,(alist-ref 'id t) "/edit")
                                                                 (data-iconpos "right")
                                                                 (data-icon "gear"))
                                                              "Edit"))
                                                       (li (a (@ (href "/tasks" ,(alist-ref 'id t) "/delegate")
                                                                 (data-iconpos "right")
                                                                 (data-icon "forward"))
                                                              "Delegate"))
                                                       (li (a (@ (href "/tasks" ,(alist-ref 'id t) "/done")
                                                                 (data-iconpos "right")
                                                                 (data-icon "check"))
                                                              "Done"))))))
                                      tasks))
                          `(a (@ (href "#login") (data-icon "back")) 
                              "Change user")))))))))

(define (get-tasks continue)
  (send-tasks-response select-tasks))

(define (get-user-tasks continue user)
  (send-tasks-response
   (lambda (group-by filter include-done)
     (let* ((user (string->user-name user))
            (tasks (select-user-tasks user group-by filter include-done)))
       (if (or (not tasks) (null? tasks))
           (and (db-select-one 'users 'name user 'id) '())
           tasks)))
   user))

(define (post-user-tasks continue user)
  (parameterize ((user-map (select-users)))
    (call/cc 
     (lambda (exit)
       (or (and-let* ((headers (request-headers (current-request)))
                      (content-length (header-value 'content-length headers))
                      (body (read-string content-length (request-port (current-request))))
                      (tasks (http-accept-case (current-request)
                               ((application/json) 
                                (read-json-tasks body))
                               ((text/x-downtime)
                                (with-input-from-string body downtime-read))))
                      (_ (unless (list? tasks) (exit #t))) ;; FIXME: not very pretty
                      (conflicts (persist-user-tasks (string->user-id user) tasks)))

             (if (null? conflicts)
                 (send-response code: 200 reason: "OK")
                 (send-response code: 409 reason: "Conflict" body: (conflicts->string conflicts (string->user-name user)))))
           (send-response code: 500
                          reason: "Internal Server Error"
                          body: "Error handling input data"))))))

(handle-exception
 (lambda (exn chain)
   (log-to (error-log) "~A" (build-error-message exn chain #t))
   (let ((message (get-condition-property exn 'exn 'message)))
     (send-response code: 500 reason: "Internal Server Error" body: message))))

(define handle-request
  (uri-match/spiffy 
   `(((/ "inspect")
      (GET ,(lambda (continue) 
	      (send-response body: (with-output-to-string 
				       (cut pp (request-headers (current-request))))))))

     ((/ "tasks")
      (GET ,get-tasks))

     ((/ "users") 
      ((/ (submatch (+ any)))
       ((/ "tasks")
	(GET ,get-user-tasks)
	(POST ,post-user-tasks))))

     ((/ "tasks")
      (GET ,(lambda (continue)
              (let ((user ((request-vars source: 'query-string) 'user)))
                (get-user-tasks continue user))))))))

(define (pox-handler continue)
  (with-db-connection (lambda ()
			(handle-request continue))))

)