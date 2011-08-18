(module pox-model 

(user-map
 user-name->id 
 user-id->name
 string->user-name
 string->user-id
 select-users
 persist-user-tasks
 select-user-tasks
 conflicts->string
 task-list->string
 with-db-connection)

(import chicken scheme ports srfi-1 srfi-13 data-structures extras)
(require-library regex)
(import irregex)
(use matchable postgresql sql-null spiffy srfi-18)
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
  (and-let* ((name name)
	     (user (find (lambda (user) (string=? name (cdr user))) (user-map))))
    (car user)))

(define (user-id->name id)
  (alist-ref id (user-map)))

(define (string->user-name user)
  (if (irregex-match '(+ num) user) (user-id->name user) user))

(define (string->user-id user)
  (if (irregex-match '(+ num) user) 
      (string->number user)
      (user-name->id user)))

(define (remove-sql-nulls l)
  (remove (compose sql-null? cdr) l))

(define (select-tasks #!optional (additional '()) (vars '()))
  (db-query (db-compose-query
             '(select (columns t.id t.revision t.name t.description t.priority t.done t.category
                               (as u1.name assignee) (as u2.name assigner) (as u3.name creator))
                (from (join left
                            (join left
                                  (join left 
                                        (as tasks t) 
                                        (as users u1) 
                                        (on (= t.assignee_id u1.id))) 
                                  (as users u2) 
                                  (on (= t.assigner_id u2.id)))
                            (as users u3)
                            (on (= t.creator_id u3.id)))))
             additional) 
	    vars))

(define (select-user-tasks user #!optional groups (filters '()) (include-done #f))
  (let* ((conditions '(in $1 #(u1.name u2.name u3.name)))
         (conditions (if (pair? filters)
			 `(and (or . ,(append-map (lambda (i)
                                                    (let ((var (string->symbol (sprintf "$~A" i))))
                                                      `((like t.name ,var)
                                                        (like t.description ,var)
                                                        (like t.category ,var))))
                                                  (iota (length filters) 2)))
                               ,conditions)
                         conditions))
	 (conditions (if include-done 
			 conditions
                         `(and (= done #f) ,conditions)))
	 (tasks (select-tasks `((order (desc t.priority) (asc t.created_at) (asc t.id))
                                (where ,conditions))
			      (cons user (map (cut sprintf "%~A%" <>) filters))))
	 (tasks (map remove-sql-nulls (result->alists tasks))))

    (tasks-group-by groups tasks)))

(define (tasks-group-by groups tasks)
  (if (and groups (pair? groups))
      (let ((group (car groups)))
	(cons* 'group
	       group
	       (map (lambda (pair)
		      (cons (car pair) (tasks-group-by (cdr groups) (cdr pair))))
		    (fold-right (lambda (task result)
				  (let ((value (alist-ref group task)))
				    (alist-update! value (cons task (alist-ref value result equal? '()))
						   result equal?))) '() tasks))))
      tasks))

(define (->number s)
  (and s (string->number (->string s))))


(define (ppp e) (pp e) e)

(define (select-user-notifications)
  (fold (lambda (n result)
	  (let ((user-id (alist-ref 'user_id n)))
	    (alist-update! user-id
			   (cons (cons (notification-ref (string->symbol (alist-ref 'name n))) (alist-ref 'params n))
				 (or (alist-ref user-id result) '()))
			   result
			   eq?)))
	'()
	(result->alists (db-query "SELECT un.user_id, un.params, nh.name
                                   FROM user_notifications AS un 
                                   JOIN notification_handlers AS nh ON nh.id = un.handler_id"))))

(define (tasks-diff? task1 task2)
  (any (lambda (col)
	 (not (equal? (alist-ref col task2)
		      (alist-ref col task1))))
       '(description name done assignee_id assigner_id priority category)))

(define (task-with-user-names task)
  (fold (lambda (field task)
	  (alist-update! (string->symbol field) 
			 (alist-ref (alist-ref (string->symbol (conc field "_id")) task) (user-map))
			 task))
	task
	'("assigner" "assignee" "creator" "updater")))

(define (task-notifyees task)
  (define (user-field? f)
    (member (car f) '(assigner_id assignee_id creator_id updater_id)))
  (delete-duplicates (map cdr (filter user-field? task))))

(define (format-error e)
  (format "Error: ~A~A" 
	  (get-condition-property e 'exn 'message)
	  (let ((args (get-condition-property e 'exn 'arguments)))
	    (if (null? args)
		""
		(apply conc (cons ": " args))))))

(define (task-notify notifications action task old-task)
  (thread-start! 
   (lambda ()
     (with-db-connection 
      (lambda ()
	(parameterize ((user-map (select-users)))
	  (let ((task     (and task (remove-sql-nulls (task-with-user-names task))))
		(old-task (and old-task (remove-sql-nulls (task-with-user-names old-task)))))

	    (for-each (lambda (notification)
			(let ((user (car notification)))
			  (for-each (lambda (n)
				      (condition-case 
					  ((car n) 
					   user 
					   (if (string? (cdr n))
					       (with-input-from-string (cdr n) read-file)
					       '()) 
					   action
					   task
					   old-task)
					  (exn () (log-to (error-log)
							  "Error with notification for ~A: ~A"
							  user
							  (format-error exn)))))
				    (cdr notification))))
		      (filter identity
			      (map (cut assq <> notifications)
				   (task-notifyees task)))))))))))

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
		      task)))

	 (insert (lambda (task user-id)
		   (alist->ssql-insert 'tasks (alist-delete 'revision (alist-update! 'creator_id user-id task)))))

	 (update (lambda (task user-id)
		   (alist->ssql-update 'tasks task conditions: `(= revision ,(sub1 (alist-ref 'revision task))))))

	 (save   (lambda (user-id task notifications)
		   (let* ((old-task  (alist-ref 'id task))
			  (old-task  (and old-task (db-select 'tasks 'id old-task)))
			  (old-task  (and old-task (row-alist old-task)))
			  (task      (prepare user-id task old-task))
			  (action    (if (alist-ref 'id task)
					 (and (tasks-diff? old-task task) 'update)
					 'insert))
			  (statement (case action
				       ((update) (update task user-id))
				       ((insert) (insert task user-id))
				       (else #f)))
			  (result    (and statement 
					  (db-query (db-compose-query statement '((returning id revision))))))
			  (task      (if (and result (not (zero? (row-count result))))
					 (alist-merge (row-alist result) task)
					 task))
			  (affected  (and result (affected-rows result))))

		     (or (not affected)
			 (and (= 1 affected)
			      (begin
				(task-notify notifications action task old-task)
				#t)))))))

    (lambda (user-id tasks)
      (with-transaction (db-connection)
        (lambda ()
          (let ((notifications (select-user-notifications)))
            (fold-right (lambda (task conflicts)
                          (if (save user-id task notifications)
                              conflicts
                              (let* ((new-task (select-tasks '(where (= t.id $1)) (list (alist-ref 'id task))))
                                     (new-task (and (< 0 (row-count new-task)) (row-alist new-task)))
                                     (task (if new-task
                                               (alist-update! 'revision (alist-ref 'revision new-task) task)
                                               task)))
                                (cons (list task new-task) conflicts))))
                        '()
                        tasks)))))))

(define (task-list->string tasks user #!optional origin)
  (with-output-to-string (cut downtime-write tasks user origin)))

(define (conflicts->string conflicts user)
  (string-intersperse (map (lambda (conflict) 
			     (string-intersperse (map (lambda (task) 
							(task->string (remove-sql-nulls task) user)) conflict)
						 "\n# ---\n\n"))
			   conflicts)
		      "\n\n# =====\n\n"))

)