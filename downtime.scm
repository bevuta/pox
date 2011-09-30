(module downtime

(downtime-read downtime-write task->item-line task->string)

(import chicken scheme extras data-structures)
(use srfi-1 srfi-13 ports matchable uri-common sexpressive srfi-14)

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

(define (irregex-match-substrings m)
  (let ((num (irregex-match-num-submatches m)))
    (let loop ((i 1))
      (if (> i num)
          '()
          (cons (irregex-match-substring m i)
                (loop (+ i 1)))))))

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
		   data))
         (tags (append (or (alist-ref 'tags task) '())
                       (or (alist-ref 'tags data) '())))
         (data (if (null? tags)
                   data
                   (alist-update! 'tags tags data))))
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
(define ignored-attributes (make-parameter '()))
(define preamble-meta (make-parameter '()))
(define scope-stack (make-parameter '()))
(define scope (make-parameter '()))
(define last-item (make-parameter #f))

(define (scope-stack-push e)
  (scope-stack (cons e (scope-stack)))
  (scope (fold task-update '() (scope-stack))))

(define (scope-stack-pop n)
  (scope-stack (drop (scope-stack) n)))

(define +blank+ (irregex '(seq (* space))))

(define (butlast+last lst)
  (if (null? lst)
      (error 'butlast+last "null argument")
      (let loop ((head '()) (lst lst))
        (if (null? (cdr lst))
            (values (reverse head) (car lst))
            (loop (cons (car lst) head) (cdr lst))))))

(define (->number n)
  (if (number? n) n (string->number (->string n))))

(define (update-alist-ref! key alist proc #!optional default (test eq?))
  (alist-update! key (proc (alist-ref key alist test default)) alist test))

(define editable-task-attributes
  '(assignee assigner priority done category tags description name))

(define (editable-task-attribute? attr)
  (memq attr editable-task-attributes))

(define read-command
  (let ((command-name-syntax (syntax:symbols))
        (argument-list-syntax (wrap-syntax (syntax:lists '((#\( . #\))))
                                           (append (syntax:whitespace)
                                                   (syntax:strings)
                                                   (syntax:symbols)))))
    (lambda (#!optional (in (current-input-port)))
      (condition-case
          (cons (read* in command-name-syntax)
                (read* in argument-list-syntax))
        (exn (exn)
             (and (not (eq? 'read* (get-condition-property exn 'exn 'location)))
                  (error exn)))))))

(define (command:ignore _ meta . keys)
  (for-each (lambda (key)
              (unless (editable-task-attribute? key)
                (error 'downtime-read "invalid task attribute" key)))
            keys)

  (update-alist-ref! 'filters
                     meta
                     (lambda (filters)
                       (cons (lambda (task)
                               (fold alist-delete task keys))
                             filters))
                     '()))

(define parse-meta
  (let* ((assign-attr (lambda (#!optional (conversion identity))
                        (lambda (key meta . vals)
                          (alist-update! key (apply conversion vals) meta))))
         (commands `((assignee . ,(assign-attr ->string))
                     (assigner . ,(assign-attr ->string))
                     (priority . ,(assign-attr ->number))
                     (done     . ,(assign-attr
                                   (lambda (val)
                                     (string=? val "done"))))
                     (category . ,(assign-attr
                                   (lambda (val)
                                     (and (not (string=? val "uncategorized")) val))))
                     (tags     . ,(lambda (key meta val)
                                    (update-alist-ref! key
                                                       meta
                                                       (lambda (tags)
                                                         (cons val tags))
                                                       '())))
                     (ignore   . ,command:ignore)))
         (command-ref (lambda (name)
                        (let ((command (alist-ref name commands)))
                          (if command
                              (lambda args
                                (apply command name args))
                              (error 'parse-meta "invalid command" name)))))
         (tokens `(((seq ">" (* space) (submatch (+ (~ space))))
                    . ,(command-ref 'assignee))
                   ((seq "<" (* space) (submatch (+ (~ space))))
                    . ,(command-ref 'assigner))
                   ((seq (submatch ("+-") (= 1 numeric)))
                    . ,(command-ref 'priority))
                   ((seq ":" (submatch (+ (~ space))))
                    . ,(command-ref 'tags))
                   ((seq (submatch (or "done" "to do")))
                    . ,(command-ref 'done))
                   ((seq (submatch (or "uncategorized" (seq "/" (+ (~ space))))))
                    . ,(command-ref 'category))
                   ((seq "@" (submatch (* any)))
                    ,(lambda (rest)
                       (call-with-input-string rest
                         (lambda (in)
                           (values (or (read-command in)
                                       (error "invalid command syntax" (string-append "@" rest)))
                                   (read-string #f in)))))
                    . ,(lambda (meta command . args)
                         (apply (command-ref command) meta args)))))
         (tokens (map (lambda (token)
                        (cons (irregex `(seq (* space) ,(car token) (submatch (* any))))
                              (cdr token)))
                      tokens)))

    (lambda (meta-line)
      (and meta-line
           (let next ((rest meta-line) (result '()))
             (if (irregex-match +blank+ rest)
                 result
                 (let try-parse ((tokens tokens))
                   (if (null? tokens)
                       (error "invalid meta data" meta-line)
                       (let* ((match (irregex-match (caar tokens) rest))
                              (match (and match (irregex-match-substrings match))))
                         (if match
                             (let ((update (cdar tokens)))
                               (if (pair? update)
                                   (receive (args rest) ((car update) (car match))
                                     (next rest (apply (cdr update) result args)))
                                   (receive (args rest) (butlast+last match)
                                     (next rest (apply update result args)))))
                             (try-parse (cdr tokens))))))))))))

(define parse-line
  (let* ((description  (irregex '(seq (submatch (or (seq (~ "*") (* any))
						    (seq "**" (* any)))))))
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
      (or (cond ((irregex-match +blank+ line) => 
		 (lambda _
		   (if (eq? 'description (last-item))
		       (cons (append-task-description (car result) "") (cdr result))
		       result)))

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

			 (error 'parse-line "illegal nesting" line)))))

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
                     (error 'parse-line "description before item" line))

                   (last-item 'description)
                   (cons (append-task-description (car result)
                                                  (string-trim-right (irregex-match-substring description 1)))
                         (cdr result))))

		
		(else #f))
	  (error 'parse-line "invalid line" line)))))


(define (apply-filters tasks)
  (map (lambda (task)
         (let ((filters (append (or (alist-ref 'filters (preamble-meta)) '())
                                (or (alist-ref 'filters task) '())))
               (task (alist-delete 'filters task)))
           (fold (lambda (filter task)
                   (filter task))
                 task
                 filters)))
       tasks))

(define apply-preamble-command
  (let ((commands `((origin . ,(lambda (preamble command uri)
                                 (alist-cons command (->string uri) preamble)))
                    (ignore . ,(lambda (preamble command . attrs)
                                 ;; this is not Haskell after all
                                 (preamble-meta (apply command:ignore command (preamble-meta) attrs))
                                 (update-alist-ref! command
                                                    preamble
                                                    (lambda (ignore)
                                                      (delete-duplicates (append attrs ignore)))
                                                    '()))))))
    (lambda (command preamble)
      (condition-case
          (apply (or (alist-ref (car command) commands)
                     (error 'downtime-read
                            "invalid preamble command"
                            (car command)))
                 preamble
                 command)
        (exn (exn arity)
             (error 'downtime-read 
                    (get-condition-property exn 'exn 'message)
                    (car command)))))))

(define (read-preamble)
  (let loop ((preamble '()))
    (let ((char (peek-char)))
      (cond ((eq? char #\@)
             (read-char)
             (loop (apply-preamble-command (read-command) preamble)))
            ((char-set-contains? char-set:whitespace char)
             (read-char)
             (loop preamble))
            (else (reverse preamble))))))

(define (downtime-read)
  (parameterize ((scope-stack '())
		 (scope '())
		 (last-item #f)
                 (preamble-meta '()))
    (cons* 'downtime
           (read-preamble)
           (apply-filters
            (unindent-descriptions
             (reverse (let next ((result '()))
                        (let ((line (read-line)))
                          (if (eof-object? line)
                              result
                              (next (parse-line line result)))))))))))



;; writing


(define (filter-scope attr)
  (filter (lambda (scope)
            (eq? attr (car scope)))
          (scope)))

(define (scoped? attr)
  (pair? (filter-scope attr)))

(define (ignored? attr)
  (memq attr (ignored-attributes)))

(define (task-priority->string task)
  (and (not (scoped? 'priority))
       (let ((priority (alist-ref 'priority task)))
	 (and priority (not (zero? priority)) 
	      (format-task-attribute 'priority priority)))))

(define (task-category->string task)
  (and (not (scoped? 'category)) 
       (alist-ref 'category task)))

(define (task-tags->string task)
  (let* ((scope-tags (map caadr (filter-scope 'tags)))
         (tags       (or (alist-ref 'tags task) '()))
         (tags       (lset-difference equal? tags scope-tags)))
    (and (not (null? tags))
         (string-intersperse 
          (map (lambda (tag)
                 (format-task-attribute 'tags tag))
               tags)))))

(define (task-assignment->string task)
  (let* ((meta (fold (lambda (attr meta)
                       (let ((name (alist-ref attr task)))
                         (if (or (not name)
                                 (equal? (current-user) name)
                                 (scoped? attr))
                             meta
                             (cons (format-task-attribute attr name) meta))))
                     '()
                     '(assignee assigner))))
    (and (pair? meta) (string-intersperse meta " "))))

(define (task-done->string t)
  (and (alist-ref 'done t) "done"))

(define (format-task-attribute attr value)
  (and (not (ignored? attr))
       (case attr
         ((priority) (conc (if (or (zero? value) (positive? value)) '+ '-) (abs value)))
         ((assigner) (conc "< " value))
         ((assignee) (conc "> " value))
         ((category) (or value "uncategorized"))
         ((done)     (if value "done" "to do"))
         ((tags)     (sprintf ":~A" value)))))

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
           (line (conc-if (task-tags->string task) line))
	   (line (conc-if (task-category->string task) line))
	   (line (conc-if (task-done->string task) line)))
      (conc line "  "))))

(define (task->string task #!optional (user (current-user)))
  (parameterize ((current-user user))
    (let* ((line (conc "* " (task->item-line task user))) 
	   (description (alist-ref 'description task)))
      (if (and description (not (ignored? 'description)))
	  (conc line "\n  " (string-intersperse (string-split description "\n" #t) "  \n  ") "\n")
	  line))))

(define (origin->string o)
  (if (string? o) o (uri->string o)))

(define (downtime-write-internal tasks)
  (match tasks
    (('group group groupings ...)
     (parameterize ((scope (alist-cons group groupings (scope))))
       (let ((heading (make-string (length (scope)) #\#)))
	 (for-each (lambda (grouping)
                     (parameterize ((ignored-attributes '()))
                       (print heading " " (format-task-attribute group (car grouping))))
		     (downtime-write-internal (cdr grouping)))
		   groupings))))
    (else (for-each (lambda (task)
		      (print (task->string task))) tasks)
	  (newline))))

(define write-preamble
  (let ((commands `((origin . ,(lambda (_ origin)
                                 (and origin (list (origin->string origin)))))
                    (ignore . ,(lambda (_ . args)
                                 (and (pair? args) args)))
                    (user   . ,(lambda (_ user) (and user (list (string->symbol user))))))))
    (lambda (preamble skip)
      (let ((printed? #f))
        (for-each (lambda (command)
                    (and-let* ((handler (and (not (memq (car command) skip))
                                             (or (alist-ref (car command) commands)
                                                 (error 'downtime-write "invalid preamble command" (car command)))))
                               (args (apply handler
                                            (car command)
                                            (if (list? (cdr command))
                                                (cdr command)
                                                (list (cdr command)))))
                               (args (if (null? args) "" (sprintf "~S" args))))
                      (printf "@~A~A~%" (car command) args)
                      (set! printed? #t)))
                  preamble)
        (when printed?
          (newline))))))

(define (downtime-write doc #!optional skip-preamble-commands)
  (if (not (eq? 'downtime (car doc)))
      (error 'downtime-write "missing downtime tag" doc)
      (let ((preamble (cadr doc)))
        (parameterize ((scope '())
                       (current-user (alist-ref 'user preamble))
                       (ignored-attributes (or (alist-ref 'ignore preamble) '())))

          (write-preamble preamble skip-preamble-commands)
          (downtime-write-internal (cddr doc))))))

)

