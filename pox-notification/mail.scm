(module pox-notification/mail

(notification-mail-from attach-diff-to-notification-mails?)

(import chicken scheme)
(use extras ports posix data-structures srfi-1
     pox-notification pox-mail pox-model downtime format-textdiff)

(define attach-diff-to-notification-mails? (make-parameter #f))

(define (diff-strings old new)
  (let* ((strings (map (lambda (t)
                         (string-split t "\n"))
                       (list new old)))
         (hunks (apply textdiff (append strings '(3)))))
    (receive (in out pid)
        (process "filterdiff -v --format=unified")
      ((make-format-textdiff 'context) out hunks
       "old" "" "new" "")
      (close-output-port out)
      (read-string #f in))))

(define (changes->string changes user)
  (let ((new-tasks (task-list->string (map second changes) user))
        (old-tasks (task-list->string (filter identity (map third changes)) user)))
    (values new-tasks (and (attach-diff-to-notification-mails?)
                           (diff-strings new-tasks old-tasks)))))

(define notification-mail-from (make-parameter #f))

(register-notification-handler 'mail
  (lambda (user notifyee params changes)
    (receive (new-tasks diff)
        (changes->string changes notifyee)
      (send-mail To:      (alist-ref 'recipient params)
                 From:    (or (alist-ref 'from params) (notification-mail-from))
                 Subject: (sprintf (or (alist-ref 'subject params) "[pox] changes by ~A") user)
                 Body:    new-tasks
                 Attachments: (and (attach-diff-to-notification-mails?)
                                   `((File: "changes.diff" Body: ,diff)))))))

)