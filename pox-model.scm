(module pox-model 

(user-map
 user-name->id 
 user-id->name
 string->user-name
 string->user-id
 select-users
 select-tasks
 persist-user-tasks
 select-user-tasks
 conflicts->string
 task-list->string
 task-list->json-serializable
 read-json-tasks
 with-db-connection
 maybe-create-user)

(import chicken scheme ports srfi-1 srfi-13 data-structures extras)
(require-library regex)
(import irregex)
(use matchable postgresql sql-null spiffy srfi-18 medea)
(use downtime)
(use pox-db pox-db/helpers)
(use pox-notification)

(define (alist-merge l1 l2)
  (fold-right (lambda (a l)
		(alist-update! (car a) (cdr a) l))
	      (alist-copy l2)
	      l1))

(define user-map (make-parameter '()))

(define (select-users)
  (row-map (lambda (user)
	     (cons (car user) (cadr user)))
	   (db-query '(select (columns id name) (from users)))))

(define (user-name->id name)
  (and name
       (let ((user (find (lambda (user)
                           (string=? name (cdr user)))
                         (user-map))))
         (if user
             (car user)
             (error (sprintf "user does not exist: ~A" name) name)))))

(define (user-id->name id)
  (alist-ref id (user-map)))

(define (string->user-name user)
  (if (irregex-match '(+ num) user) (user-id->name user) user))

(define (string->user-id user)
  (if (irregex-match '(+ num) user) 
      (string->number user)
      (user-name->id user)))

(define (maybe-create-user name)
  (condition-case
      (db-query '(insert (into users) (columns name) (values #($1))) (list name))
    (exn (postgresql)
         (unless (equal? "23505" (get-condition-property exn 'postgresql 'error-code))
           (signal exn)))))

(define (remove-sql-nulls l)
  (remove (compose sql-null? cdr) l))

(define tasks-query
  `(select (columns creator  creator_id
                    assigner assigner_id
                    assignee assignee_id
                    updater  updater_id
                    id
                    created_at
                    revision
                    name
                    description
                    priority
                    done
                    category
                    tags)
     (from tasks_with_tags)))

(define (select-tasks #!optional (additional '()) (vars '()))
  (db-query (db-compose-query tasks-query additional) vars))

(define (select-task id)
  (db-query (db-compose-query tasks-query `((where (= id ,id))))))

(define (prepare-filters filters)
  (partition (lambda (filter)
               (eq? #\: (string-ref filter 0)))
             (filter (lambda (filter)
                       (> (string-length filter) 1))
                     filters)))

(define (select-tasks groups filters include-done #!optional additional-conditions)
  (receive (tag-filters filters)
      (prepare-filters (map symbol->string filters))
    (let* ((conditions (if (pair? filters)
                           `((and . ,(map (lambda (i)
                                            (let ((var (string->symbol (sprintf "$~A" i))))
                                              `(or (like name ,var)
                                                   (like description ,var)
                                                   (like category ,var))))
                                          (iota (length filters) 1))))
                           '()))
           (tag-filters (map (lambda (f) (string-drop f 1)) tag-filters))
           (conditions (if (pair? tag-filters)
                           `((and (@> tags (array . ,tag-filters))
                                  . ,conditions))
                           conditions))
           (conditions (if include-done 
                           conditions
                           `((and (= done #f) . ,conditions))))

           (conditions (if additional-conditions
                           `((and ,additional-conditions . ,conditions))
                           conditions))
           (result (db-query (db-compose-query tasks-query
                                               `((order (desc priority) (asc created_at) (asc id))
                                                 . ,(if (null? conditions) 
                                                        '()
                                                        `((where . ,conditions)))))
                             (map (cut sprintf "%~A%" <>) filters)))
           (tasks (result->tasks result #t)))

      (tasks-group-by groups tasks))))

(define (select-user-tasks user groups filters include-done)
  (select-tasks groups filters include-done `(in ,user #(assignee assigner creator))))

(define (tasks-group-by groups tasks)
  (if (and groups (pair? groups))
      (let* ((group     (car groups))
             (group-str (symbol->string group))
             (tag       (and (eq? #\: (string-ref group-str 0))
                             (string-drop group-str 1)))
             (group     (if tag 'tags group)))
	(cons* 'group
	       group
	       (map (lambda (pair)
		      (cons (car pair) (tasks-group-by (cdr groups) (cdr pair))))
                    (fold-right (lambda (task result)
                                  (let* ((value     (alist-ref group task))
                                         (included? (or (not tag) (and value (member tag value equal?))))
                                         (value     (if (and tag included?)
                                                        tag
                                                        value)))
                                    (if included?
                                        (alist-update! value 
                                                       (cons task (alist-ref value result equal? '()))
                                                       result equal?)
                                        result)))
                                '()
                                tasks))))
      tasks))

(define (->number s)
  (and s (string->number (->string s))))

(define (select-user-notifications)
  (fold (lambda (n result)
	  (let ((user-id (alist-ref 'user_id n)))
	    (alist-update! user-id
			   (cons (cons (notification-ref (string->symbol (alist-ref 'name n)))
                                       (alist-ref 'params n))
				 (or (alist-ref user-id result) '()))
			   result
			   eq?)))
	'()
	(result->alists (db-query '(select (columns un.user_id un.params nh.name)
                                     (from (join inner
                                                 (as user_notifications un)
                                                 (as notification_handlers nh)
                                                 (on (= nh.id un.handler_id)))))))))

(define (tasks-diff? task1 task2)
  (or (any (lambda (col)
             (not (equal? (alist-ref col task2)
                          (alist-ref col task1))))
           '(description name done assignee_id assigner_id priority category))
      (not (lset= equal? 
                  (or (alist-ref 'tags task1) '())
                  (or (alist-ref 'tags task2) '())))))

;; FIXME: this is probably not required anymore as tasks should always contain user names now
;; ok, we still need for adding the names back in after saving a task
(define (task-with-user-names task)
  (fold (lambda (field task)
	  (alist-update! field 
			 (alist-ref (alist-ref (string->symbol (sprintf "~A_id" field)) task) (user-map))
			 task))
	task
	'(assigner assignee creator updater)))

(define (task-user-id-field? f)
  (memq (car f) '(assigner_id assignee_id creator_id updater_id)))

(define (task-notifyees task)
  (delete-duplicates (map cdr (filter task-user-id-field? task))))

(define (format-error e)
  (format "Error: ~A~A" 
	  (get-condition-property e 'exn 'message)
	  (let ((args (get-condition-property e 'exn 'arguments)))
	    (if (null? args)
		""
		(apply conc (cons ": " args))))))

(define (group-changes-by-notifyees user-id changes)
  (fold (lambda (change notifyees)
          (fold (lambda (notifyee notifyees)
                  (alist-update! notifyee
                                 (cons change
                                       (or (alist-ref notifyee notifyees) '()))
                                 notifyees))
                notifyees
                (delete user-id (task-notifyees (cadr change)))))
        '()
        changes))

;; Notifies all affected users about changes (a list of the structure
;; (<event> <old-task> <new-task>)) with event being either `insert'
;; or `update'.
;; 
;; TODO: Should perhaps also notify users that *were* affected in the
;; old-task but aren't anymore in new-task. Currently only users
;; affected in new-task are notified.
(define (notify-users user-id changes)
  (thread-start! 
   (lambda ()
     (with-db-connection
      (lambda ()
        (let ((notifyees (group-changes-by-notifyees user-id changes))
              (user-notifications (select-user-notifications)))
          (for-each (lambda (notifyee)
                      (and-let* ((user (car notifyee))
                                 (changes (cdr notifyee))
                                 (notifications (alist-ref user user-notifications)))
                        (for-each (lambda (n)
                                    (condition-case 
                                        ((car n)
                                         user
                                         (if (string? (cdr n))
                                             (with-input-from-string (cdr n) read-file)
                                             '()) 
                                         changes)
                                      (exn () (log-to (error-log)
                                                      "Error with notification for ~A: ~S"
                                                      user
                                                      (condition->list exn)))))
                                  notifications)))
                    notifyees)))))))

(define (row-task row #!optional remove-sql-nulls? (num 0))
  (let* ((task (row-alist row num))
         (task (if remove-sql-nulls? (remove-sql-nulls task) task))
         (tags (alist-ref 'tags task)))
    (alist-update! 'tags (vector->list tags) task)))

(define (result->tasks result #!optional remove-sql-nulls?)
  (map (lambda (i)
         (row-task result remove-sql-nulls? i))
       (iota (row-count result))))

(define (select-task-tags task-id)
  (result->alists
   (db-query `(select (columns tt.id t.name)
                (from (join inner 
                            (as task_tags tt)
                            (as tags t)
                            (on (= t.id tt.tag_id))))
                (where (= tt.task_id ,task-id))))))

(define (select-tag-id tag)
  (or (db-select-one 'tags 'name tag 'id)
      (value-at (db-query `(insert (into tags) (columns name) (values #($1)) (returning id))
                          (list tag)))))

(define (persist-tags task)
  (let* ((tags (alist-ref 'tags task))
         (tags (and tags (delete-duplicates tags equal?)))
         (id   (alist-ref 'id task)))
    (if (or (not tags) (null? tags))
        (db-query `(delete (from task_tags) (where (= task_id ,id))))
        (let* ((old-tags (fold (lambda (tag old-tags)
                                 (let ((old-tags* (remove (lambda (old-tag)
                                                            (equal? tag (alist-ref 'name old-tag)))
                                                          old-tags)))
                                   (when (= (length old-tags) (length old-tags*))
                                     (db-query `(insert (into task_tags) 
                                                        (columns task_id tag_id)
                                                        (values #($1 $2)))
                                               (list id (select-tag-id tag))))
                                   old-tags*))
                               (select-task-tags id)
                               tags))
               (old-tags (map (lambda (tag) 
                                (alist-ref 'id tag))
                              old-tags)))
          (unless (null? old-tags)
            (db-query `(delete (from task_tags) 
                               (where (in id ,(list->vector old-tags))))))))))

(define persist-user-tasks
  (let* ((prepare (lambda (user-id task old-task)
		    (let* ((task (alist-merge `((description . ,(or (alist-ref 'description task) (sql-null)))
						(category    . ,(or (alist-ref 'category task) (sql-null)))
						(done        . ,(alist-ref 'done task))
						(creator_id  . ,(if old-task (alist-ref 'creator_id old-task) user-id))
						(updater_id  . ,user-id)
						(assignee_id . ,(or (user-name->id (alist-ref 'assignee task)) user-id))
						(assigner_id . ,(or (user-name->id (alist-ref 'assigner task)) user-id))
						(priority    . ,(or (->number (alist-ref 'priority task)) 0))
						(revision    . ,(add1 (or (->number (alist-ref 'revision task)) 1))))
					      task))
			   (task (alist-delete 'assignee task))
			   (task (alist-delete 'assigner task)))
		      (remove-sql-nulls task))))

	 (insert (lambda (task user-id)
                   (let* ((task (alist-update! 'creator_id user-id task))
                          (task (alist-delete 'revision task))
                          (task (alist-delete 'tags task)))
                     (alist->ssql-insert 'tasks task))))

	 (update (lambda (task user-id)
		   (alist->ssql-update 'tasks (alist-delete 'tags task)
                                       conditions: `(= revision ,(sub1 (alist-ref 'revision task))))))

	 (save   (lambda (user-id task)
		   (let* ((old-task (alist-ref 'id task))
                          (old-task (and old-task (select-task old-task)))
                          (old-task (and old-task
                                         (< 0 (row-count old-task))
                                         (row-task old-task #t)))
			  (task      (prepare user-id task old-task))
			  (action    (if old-task
					 (and (tasks-diff? old-task task) 'update)
					 'insert))
			  (statement (case action
				       ((update) (update task user-id))
				       ((insert) (insert task user-id))
				       (else #f)))
			  (result    (and statement
					  (db-query (db-compose-query (car statement) '((returning id revision)))
                                                    (cdr statement))))
			  (task      (if (and result (not (zero? (row-count result))))
					 (alist-merge (row-alist result) task)
					 task))
                          (task      (task-with-user-names task))
			  (affected  (and result (affected-rows result))))

		     (or (not affected)
			 (and (= 1 affected)
			      (begin
                                (persist-tags task)
				(list action task old-task)))))))
         (save-all (lambda (user-id tasks)
                     (fold-right (lambda (task result)
                                   (let ((change (save user-id task)))
                                     (if change
                                         (if (pair? change)
                                             (cons (cons change (car result))
                                                   (cdr result))
                                             result)
                                         (let* ((new-task (select-task (alist-ref 'id task)))
                                                (new-task (and (< 0 (row-count new-task)) (row-task new-task #t)))
                                                (task (if new-task
                                                          (alist-update! 'revision (alist-ref 'revision new-task) task)
                                                          task)))
                                           (cons (car result)
                                                 (cons (list task new-task)
                                                       (cdr result)))))))
                                 '(())
                                 tasks))))

    (lambda (user-id tasks)
      (with-transaction (db-connection)
        (lambda ()
          (let ((results (save-all user-id tasks)))
            (notify-users user-id (car results))
            (cdr results)))))))

(define (task-list->string tasks user #!optional origin)
  (with-output-to-string (cut downtime-write tasks user origin)))

(define (conflicts->string conflicts user)
  (string-intersperse (map (lambda (conflict) 
			     (string-intersperse (map (lambda (task) 
							(task->string task user)) conflict)
						 "\n# ---\n\n"))
			   conflicts)
		      "\n\n# =====\n\n"))

(define (task-list->json-serializable tasks)
  (list->vector (map (lambda (task)
                       (let* ((tags (or (alist-ref 'tags task) '()))
                              (task (alist-delete 'creator task))
                              (task (alist-delete 'creator_id task)))
                         (alist-update! 'tags (list->vector tags) task)))
                     tasks)))

(define task-json-parsers
  (alist-cons 'array identity (json-parsers)))

(define (read-json-tasks str)
  (parameterize ((json-parsers task-json-parsers))
    (read-json str)))

)