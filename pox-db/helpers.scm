(module pox-db/helpers

(alist->sql-update/insert
 alist->sql-insert
 alist->sql-update
 alist->ssql-insert
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

(define (alist->sql-update/insert table alist #!key (pkey 'id) conditions)
  (if (alist-ref pkey alist)
      (alist->sql-update table alist pkey: pkey conditions: conditions)
      (alist->sql-insert table alist)))

(define (quote-id id)
  (conc "\"" id "\""))

(define (alist->sql-insert table alist)
  (let* ((cols (string-intersperse (map (compose quote-id car) alist) ", "))
         (vals (string-intersperse (list-tabulate (length alist) 
                                                  (lambda (p) 
                                                    (conc "$" (add1 p)))) ", ")))
    (cons
     (sprintf "INSERT INTO ~A (~A) VALUES (~A)" (quote-id table) cols vals)               
     (map cdr alist))))

(define (alist->ssql-insert table alist)
  `(insert (into ,table) 
           (columns . ,(map car alist))
           (values . ,(map cdr alist))))

(define (alist->sql-update table alist #!key (pkey 'id) conditions)
  (let* ((pkey-val  (alist-ref pkey alist))
	 (alist     (alist-delete pkey alist))
	 (vals      (map-in-order cdr alist))
	 (cond-str  (and conditions (car conditions)))
	 (cond-vals (if conditions (cdr conditions) '()))
	 (vals      (append cond-vals vals (list pkey-val)))
	 (vars      (let loop ((pos (add1 (length cond-vals)))
			       (alist alist)
			       (result '()))

		      (if (null? alist)
			  result
			  (loop (add1 pos)
				(cdr alist)
				(let ((var (conc "$" pos))
				      (col (quote-id (caar alist))))
				  (cons (conc col " = " var) result))))))
	 (statement (conc "UPDATE " (quote-id table) " SET " (string-intersperse (reverse vars) ", ")
			  " WHERE " (quote-id pkey) " = $" (length vals))))

    (cons (if cond-str
	      (conc statement " AND (" cond-str ")")
	      statement)
	  vals)))

(define (alist->ssql-update table alist #!key (pkey 'id) conditions)
  (let* ((pkey-cond `(= ,pkey ,(alist-ref pkey alist)))
         (conditions (if conditions 
                         `(and ,pkey-cond ,conditions)
                         pkey-cond)))
    `(update (table ,table)
             (set . ,(map (lambda (p) 
                            (list (car p) (cdr p)))
                          (alist-delete pkey alist)))
             (where ,conditions))))


(define (db-query statement #!optional (vars '()))
  (pp (list query: statement vars))
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
                             (where (= ,condition-column ,value))))))
    (and (> (row-count result) 0) result)))

(define (result->alists result)
  (map (cut row-alist result <>) (iota (row-count result))))

)