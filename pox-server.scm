(module pox-server (pox-handler)

(import chicken scheme)
(use spiffy srfi-1 extras ports data-structures medea intarweb spiffy-request-vars
     spiffy-uri-match pox-db/helpers pox-model downtime uri-common sxml-transforms
     irregex pox-auth spiffy-middleware spiffy-auth spiffy-session pox-log)

(include "web")

(http-status-codes
 `((unprocessable-entity 422 . "Unprocessable Entity")
   . ,(http-status-codes)))

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
            (send-response status: 'not-found body: "Not Found")
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
                 (send-response status: 'no-content)
                 (send-response status: 'conflict
                                body: (conflicts->string conflicts (string->user-name user)))))
           (send-response status: 'internal-server-error
                          body: "Error handling input data"))))))

(session-cookie-name "pox-sid")

(get-session-id (lambda ()
                  (log debug: auth: session: "checking cookie")
                  (or (get-session-id-from-cookie)
                      (let ((headers (request-headers (current-request))))
                        (and (eq? 'x-session (header-value 'authorization headers))
                             (header-param 'id 'authorization headers))))))

(define (post-session continue)
  (with-request-vars* (request-vars source: 'request-body)
      ((user (nonempty as-string)) (password (nonempty as-string)))
    (if (valid-credentials? user password)
        (with-new-session
         (lambda ()
           (maybe-create-user user)
           (session-set! 'user user)
           (send-response status: 'created)))
        (send-response status: 'unprocessable-entity body: "Invalid credentials"))))

(handle-exception
 (lambda (exn chain)
   (log-to (error-log) "~A" (build-error-message exn chain #t))
   (let ((message (get-condition-property exn 'exn 'message)))
     (send-response status: 'internal-server-error body: message))))

(define (with-request-dump continue)
  (log debug: request: headers:
       (headers->list (request-headers (current-request))))
  (continue))


(define (with-authentication* handler)
  (if (authentication-enabled?)
      (chain-handlers with-authentication handler)
      handler))

(define handle-request
  (chain-handlers
   with-request-dump
   with-session
   (uri-match/spiffy
    `(((/ "session")
       (POST ,post-session))

      ((/ "users")
       ((/ (submatch (+ any)))
        ((/ "tasks")
         (GET ,(with-authentication* get-user-tasks))
         (POST ,(with-authentication* post-user-tasks)))))

      ((/ "tasks")
       (GET ,(with-authentication*
              (lambda (continue)
                (let ((user ((request-vars source: 'query-string) 'user)))
                  (if user
                      (get-user-tasks continue user)
                      (get-tasks continue)))))))))))

(define (pox-handler continue)
  (with-db-connection (lambda ()
                        (handle-request continue))))

)