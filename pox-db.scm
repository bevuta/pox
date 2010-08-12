(module pox-db

(db-connection-spec with-db-connection db-connection-pool-size db-connection)

(import chicken scheme srfi-1)
(use postgresql pool)

(define db-connection (make-parameter #f))
(define db-connection-spec (make-parameter '()))
(define db-connection-pool-size (make-parameter 2))

(define db-connection-pool 
  (delay (make-pool (map (lambda (i)
			   (connect (db-connection-spec)))
			 (iota (db-connection-pool-size))))))


(define (with-db-connection thunk)
  (call-with-value-from-pool (force db-connection-pool)
    (lambda (c)
      (parameterize ((db-connection c))
	(thunk)))))

)