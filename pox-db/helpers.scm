(module pox-db/helpers

(alist->ssql-insert
 alist->ssql-update
 quote-id
 db-query
 db-compose-query
 db-select
 db-select-one
 result->alists)

(import chicken scheme data-structures extras)
(use srfi-1 postgresql pox-db ssql)
(require-library ssql-postgresql)

(define (->sql stmt)
  (if (string? stmt)
      stmt
      (ssql->sql (db-connection) stmt)))

(define (quote-id id)
  (quote-identifier (db-connection) id))

(define (alist->ssql-insert table alist)
  (cons `(insert (into ,table) 
                 (columns . ,(map car alist))
                 (values ,(list-tabulate (length alist) 
                                         (lambda (p) 
                                           (string->symbol (sprintf "$~A" (add1 p)))))))
        (map cdr alist)))

(define (alist->ssql-update table alist #!key (pkey 'id) conditions)
  (let* ((pkey-cond `(= ,pkey ,(alist-ref pkey alist)))
         (conditions (if conditions 
                         `(and ,pkey-cond ,conditions)
                         pkey-cond))
         (alist (alist-delete pkey alist)))
    (cons `(update (table ,table)
                   (set . ,(map (lambda (col i)
                                  (list (car col) (string->symbol (sprintf "$~A" (add1 i)))))
                                alist
                                (iota (length alist))))
                   (where ,conditions))
          (map cdr alist))))

(define (db-query statement #!optional (vars '()))
  (query* (db-connection)
	  (->sql statement)
	  vars))

(define (db-compose-query a b)
  (ssql-compose (db-connection) a b))

(define (db-select-one table condition-column value column)
  (let ((result (db-select table condition-column value column)))
    (and result (value-at result))))

(define (db-select table condition-column value #!optional (columns '*))
  (let ((result (db-query `(select (columns ,columns)
                             (from ,table)
                             (where (= ,condition-column $1)))
                          (list value))))
    (and (> (row-count result) 0) result)))

(define (result->alists result)
  (map (cut row-alist result <>) (iota (row-count result))))

)