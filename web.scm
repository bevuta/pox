(define (json-response handler)
  (lambda args
    (send-response body: (with-output-to-string (cut json-write (apply handler args)))
		   headers: '((content-type #(application/json ((charset . "utf-8"))))))))

(define-syntax http-accept-case 
  (syntax-rules ()
    ((_ request (t1 e1) (t2 e2) ...)
     (lambda args
       (let ((accept-headers (header-values 'accept (request-headers request))))
	 (cond ((pair? (lset-intersection eq? accept-headers 't1)) (apply e1 args))
	       ((pair? (lset-intersection eq? accept-headers 't2)) (apply e2 args))
	       ...
	       (else (send-response code: 406 reason: "Not Acceptable"))))))))