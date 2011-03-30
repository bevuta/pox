#!/usr/bin/csi -ns

(use http-client intarweb uri-common ports matchable getopt-long data-structures tcp)
(require-library regex)
(import irregex)

(tcp-buffer-size 1024)

(define (print-usage command #!optional options)
  (print "usage: " (program-name) " " command)
  (when options
    (newline)
    (print "Options:")
    (print options)))

(define base-uri (uri-reference "http://localhost:7040/"))

(define (make-pox-uri path query)
  (update-uri base-uri
	      path: path
	      query: query))

(define (make-pox-request uri #!key (method 'GET))
  (make-request uri: uri
		method: method
		headers: (headers '((accept text/x-downtime)
				    (content-type text/x-downtime)))))

(define (port-pipe #!key (from (current-input-port)) (to (current-output-port)) (read read-char) (write write-char))
  (with-output-to-port to
    (lambda ()
      (with-input-from-port from
	(lambda ()
	  (port-for-each write read))))))

(define (get-tasks user options)
  (with-input-from-request (make-pox-request (make-pox-uri `(/ "users" ,user "tasks") options))
			   #f
			   (cut port-pipe read: read-line write: print)))


(define get-tasks-options-grammar
  '((omit-origin "omit the @origin declaration"
		 (required #f)
		 (single-char #\o)
		 (value #f))
    (include-done "include tasks marked as done"
		  (required #f)
		  (single-char #\d)
		  (value #f))))

(define (get-tasks-options options)
  (cdr (getopt-long options get-tasks-options-grammar)))


(define (post-tasks options)
  (let* ((file (alist-ref 'file options))
	 (port (if file (open-input-file file) (current-input-port))))
    (and-let* ((origin (read-line port))
	       (origin (irregex-match '(seq "@origin" (+ space) (submatch (+ any))) origin))
	       (origin (irregex-match-substring origin 1))
	       (origin (string-trim origin)))
      (with-input-from-request (make-pox-request (uri-reference origin) method: 'POST)
			       (read-string #f port)
			       (cut port-pipe)))

    (when file (close-input-port port))))


(define post-tasks-options-grammar
  '((file "read tasks from FILE instead of standard input"
	  (required? #f)
	  (single-char #\f)
	  (value (required FILE)))))

(define (post-tasks-options options)
  (cdr (getopt-long options post-tasks-options-grammar)))


(match (command-line-arguments)
  (("get" user . options)
   (get-tasks user (get-tasks-options options)))
  (("get" . ...) (print-usage "get USER" (usage get-tasks-options-grammar)))

  (("post" . options) (post-tasks (post-tasks-options options)))
  
  (else (print-usage "COMMAND [OPTION ...]")
	(newline)
	(print "Available commands:")
	(print " get     Get tasks for a given user")
	(print " post    Post a task list back to the server")
	(newline)))