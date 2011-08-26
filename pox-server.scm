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

(define (get-user-tasks continue user)
  (parameterize ((user-map (select-users)))
    (with-request-vars* (request-vars source: 'query-string)
        (group-by filter (include-done as-boolean) (omit-origin as-boolean))
      (let* ((user (string->user-name user))
             (tasks (select-user-tasks 
                     user 
                     (list-string->list group-by)
                     (list-string->list filter)
                     include-done)))
        (http-accept-case (current-request)
          ((application/json) 
           (send-json-response (task-list->json-serializable tasks)))
          ((text/x-downtime)
           (if (or (not tasks) (null? tasks))
               (if (db-select-one 'users 'name user 'id)
                   (send-response code: 200 body: "")
                   (send-response code: 404 reason: "Not Found"))
               (send-response headers: '((content-type #(text/x-downtime ((charset . "utf-8")))))
                              body: (task-list->string 
                                     tasks
                                     user 
                                     (and (not omit-origin)
                                          (request-uri (current-request)))))))
          ((text/html)
           (send-page "tasks"
                      `("Tasks of " ,user)
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
                          "Change user"))))))))

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
	(POST ,post-user-tasks))))

     ((/ "tasks")
      (GET ,(lambda (continue)
              (let ((user ((request-vars source: 'query-string) 'user)))
                (get-user-tasks continue user))))))))

(define (pox-handler continue)
  (with-db-connection (lambda ()
			(handle-request continue))))

)