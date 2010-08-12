(module downtime

(downtime-read downtime-write task->item-line task->string)

(import chicken scheme extras data-structures)
(use srfi-1 srfi-13 ports matchable)

(require-library regex)
(import irregex)

;; reading

(define (irregex-match->alist m fields)
  (fold (lambda (field task)
	  (let ((value (irregex-match-substring m field)))
	    (if value
		(alist-update! field value task)
		task)))
	'()
	fields))

(define (irregex-match->task m)
  (irregex-match->alist m '(id revision name meta)))

(define (append-task-description task description)
  (alist-update! 'description 
		 (conc (or (alist-ref 'description task) "")
		       description
		       "\n")
		 task))

(define (alist-merge l1 l2)
  (fold-right (lambda (a l)
		(alist-update! (car a) (cdr a) l))
	      (alist-copy l2)
	      l1))

(define (strip-trailing-slash s)
  (string-trim-right s #\/))

(define (category-absolute? c)
  (and (> (string-length c) 1)
       (string=? (substring c 0 2) "//")))

(define (categories-merge c1 c2)
  (let ((c2 (and c2 (strip-trailing-slash c2))))
    (if (and c2 (category-absolute? c2))
	(substring c2 1)
	(if c1 (if c2 (conc c1 c2) c1) c2))))

(define (task-update task data)
  (let* ((category (categories-merge (alist-ref 'category task)
				     (alist-ref 'category data)))
	 (data (if category 
		   (alist-update! 'category category (alist-copy data))
		   data)))
    (alist-merge data task)))

(define (alist-convert! key conversion alist)
  (if (alist-ref key alist)
      (alist-update! key (conversion (alist-ref key alist)) alist)
      alist))

(define (unindent str)
  (let* ((lines (string-split str "\n" #t))
         (ws (reduce min 0
                     (map (lambda (x)
                            (or (string-skip x #\space) 0))
                          (remove (cut string=? "" <>) lines)))))

    (if (= 0 ws)
	str
	(string-intersperse
	 (map (lambda (x) 
		(if (string=? "" x) x (substring x ws)))
	      lines)
	 "\n"))))

(define (unindent-descriptions tasks)
  (map (lambda (task)
	 (let ((description (alist-ref 'description task)))
	   (if description
	       (alist-update! 'description 
			      (string-trim-right (unindent description))
			      task)
	       task)))
       tasks))

(define current-user (make-parameter #f))
(define scope-stack (make-parameter '()))
(define scope (make-parameter '()))
(define last-item (make-parameter #f))

(define (scope-stack-push e)
  (scope-stack (cons e (scope-stack)))
  (scope (fold task-update '() (scope-stack))))

(define (scope-stack-pop n)
  (scope-stack (drop (scope-stack) n)))

(define parse-line
  (let* ((blank        (irregex '(seq (* space))))

	 (description  (irregex '(seq (submatch (or (seq (~ "*") (* any))
						    (seq "**" (* any)))))))

	 (parse-meta   (let* ((parts       `((assignee (seq ">" (* space) (submatch-named assignee (+ (~ space)))))
					     (assigner (seq "<" (* space) (submatch-named assigner (+ (~ space)))))
					     (priority (seq (submatch-named priority ("+-") (= 1 numeric))) ,string->number)
					     (done     (seq (submatch-named done (or "done" "to do"))) 
						       ,(lambda (d) (string=? d "done")))
					     (category (seq (submatch-named category (or "uncategorized" (seq "/" (+ (~ space))))))
						       ,(lambda (c) (if (string=? c "uncategorized") #f c)))))

			      (part-names   (map car parts))
			      (conversions  (map (lambda (part)
						   (cons (car part) (if (null? (cddr part))
									identity
									(caddr part))))
						 parts))
			      (meta         (irregex `(seq (* space) (or ,@(map cadr parts)) (submatch-named rest (* any))))))
			 
			 (lambda (meta-line)
			   (and meta-line
				(let next ((rest meta-line)
					   (result '()))
				  
				  (if (irregex-match blank rest)
				      result
				      (let ((match (irregex-match meta rest)))
					(if match
					    (next (irregex-match-substring match 'rest)
						  (alist-merge (map (lambda (pair)
								      (cons (car pair)
									    ((alist-ref (car pair) conversions) (cdr pair))))
								    (irregex-match->alist match part-names)) result))
					    (error (format "invalid meta data: ~A" meta-line))))))))))

	 (item         (let* ((name     `(seq (submatch-named name (* (~ "#")) (~ space "#")) (* space)))
			      (id       `(seq (submatch-named id (+ numeric))))
			      (revision `(seq (submatch-named revision (+ numeric)))))
			 (irregex `(seq (* space) ,name (? "#" (? ,id (? ":" ,revision)) (submatch-named meta (* any)))))))

	 (starts-with? (lambda (prefix line)
			 (and-let* ((match (irregex-match `(seq ,prefix (submatch (* any))) line)))
			   (irregex-match-substring match 1))))

	 (convert-to   (lambda (expression converter)
			 (lambda (line)
			   (and-let* ((match (irregex-match expression line)))
			     (converter match))))))

    (lambda (line result)
      (or (cond ((irregex-match blank line) => 
		 (lambda _
		   (if (eq? 'description (last-item))
		       (cons (append-task-description (car result) "") (cdr result))
		       result)))

		((starts-with? "@" line) result)

		((starts-with? "#" line) =>
		 (lambda (rest-line)
		   (and-let* ((scope-level (length (scope-stack)))
			      (match (irregex-match `(seq (submatch (* "#"))
							  (submatch (* any))) rest-line))
			      (prefix (irregex-match-substring match 1))
			      (prefix (string-length prefix)))

		     (if (<= prefix scope-level)
			 (and-let* ((meta (irregex-match-substring match 2))
				    (meta (parse-meta meta)))

			   (when (< prefix scope-level)
			     (scope-stack-pop (- scope-level prefix)))

			   (scope-stack-push meta)
			   (last-item 'heading)
			   result)

			 (error (format "illegal nesting: ~A" line))))))
		
		((starts-with? '(seq "*" (look-ahead (~ "*"))) line) => 
		 (convert-to item
			     (lambda (item)
			       (let* ((task  (irregex-match->task item))
				      (meta  (parse-meta (alist-ref 'meta task)))
				      (task  (alist-delete 'meta task))
				      (task  (if meta (task-update task meta) task))
				      (task  (alist-convert! 'revision string->number task))
				      (task  (alist-convert! 'id string->number task))
				      (cat   (and meta (alist-ref 'category meta)))
				      (scope (if (and cat (category-absolute? cat))
						 (alist-delete 'category (scope))
						 (scope)))
				      (task  (task-update scope task)))
				 (last-item 'task)
				 (cons task result)))))

		((irregex-match description line) => 
		 (lambda (description)
		   (unless (member (last-item) '(task description))
		     (error 'parse-line (format "description before item: ~A" line)))

		   (last-item 'description)
		   (cons (append-task-description (car result)
						  (string-trim-right (irregex-match-substring description 1)))
			 (cdr result))))

		
		(else #f))
	  (error 'parse-line (format "invalid line: ~A" line))))))


(define (downtime-read)
  (parameterize ((scope-stack '())
		 (scope '())
		 (last-item #f))
    (unindent-descriptions
     (reverse (let next ((result '()))
		(let ((line (read-line)))
		  (if (eof-object? line)
		      result
		      (next (parse-line line result)))))))))



;; writing

(define (task-priority->string task)
  (and (not (member 'priority (scope)))
       (let ((priority (alist-ref 'priority task)))
	 (and priority (not (zero? priority)) 
	      (format-task-attribute 'priority priority)))))

(define (task-category->string task)
  (and (not (member 'category (scope))) 
       (alist-ref 'category task)))

(define (task-assignment->string task)
  (let ((assignee (alist-ref 'assignee task))
	(assigner (alist-ref 'assigner task)))
    (and (not (equal? assignee assigner))
	 (if (not (equal? assigner (current-user)))
	     (and (not (member 'assigner (scope)))
		  (format-task-attribute 'assigner assigner))
	     (and (not (member 'assignee (scope)))
		  (format-task-attribute 'assignee assignee))))))

(define (task-done->string t)
  (and (alist-ref 'done t) "done"))

(define (format-task-attribute attr value)
  (case attr
    ((priority) (conc (if (or (zero? value) (positive? value)) '+ '-) (abs value)))
    ((assigner) (conc "< " value))
    ((assignee) (conc "> " value))
    ((category) (or value "uncategorized"))
    ((done)     (if value "done" "to do"))))

(define (conc-if s1 s2)
  (if s1 (conc s2 " " s1) s2))

(define (task->item-line task #!optional (user (current-user)))
  (parameterize ((current-user user))
    (let* ((line (format "~A #~A" (alist-ref 'name task) (alist-ref 'id task)))
	   (line (let ((revision (alist-ref 'revision task)))
		   (if (and revision (> revision 1))
		       (conc line ":" revision)
		       line)))
	   (line (conc-if (task-assignment->string task) line))
	   (line (conc-if (task-priority->string task) line))
	   (line (conc-if (task-category->string task) line))
	   (line (conc-if (task-done->string task) line)))
      (conc line "  "))))

(define (task->string task #!optional (user (current-user)))
  (parameterize ((current-user user))
    (let* ((line (conc "* " (task->item-line task user))) 
	   (description (alist-ref 'description task)))
      (if description
	  (conc line "\n  " (string-intersperse (string-split description "\n" #t) "  \n  ") "\n")
	  line))))

(define (downtime-write-internal tasks)
  (match tasks
    (('group group groupings ...)
     (parameterize ((scope (cons group (scope))))
       (let ((heading (make-string (length (scope)) #\#)))
	 (for-each (lambda (grouping)
		     (print heading " " (format-task-attribute group (car grouping)))
		     (downtime-write-internal (cdr grouping)))
		   groupings))))
    (else (for-each (lambda (task)
		      (print (task->string task))) tasks)
	  (newline))))

(define (downtime-write tasks user #!optional origin)
  (parameterize ((scope '()) (current-user user))
    (when origin (print "@origin " origin #\newline))
    (downtime-write-internal tasks)))

)