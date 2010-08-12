(module pox-db/helpers

(alist->sql-update/insert
 alist->sql-insert
 alist->sql-update
 quote-id
 db-query
 db-select
 db-select-one
 result->alists)

(import chicken scheme data-structures extras)
(use srfi-1 postgresql pox-db)

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


(define (db-query statement #!optional (vars '()))
  (query* (db-connection)
	  statement
	  vars))

(define (db-select-one table condition-column value column)
  (let ((result (db-select table condition-column value (quote-id column))))
    (and result (value-at result))))

(define (db-select table condition-column value #!optional (columns  "*"))
  (let ((result (db-query (format "SELECT ~A FROM ~A WHERE ~A = $1" 
				  columns
				  (quote-id table)
				  (quote-id condition-column))
			  (list value))))
    (and (> (row-count result) 0) result)))

(define (result->alists result)
  (map (cut row-alist result <>) (iota (row-count result))))

)