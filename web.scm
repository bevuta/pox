(include "sxml")

(define (send-sxml-response page)
  (send-response body: (sxml-page->html page)
                 headers: '((content-type #(text/html ((charset . "utf-8")))))))

(define (send-json-response doc)
  (send-response body: (json->string doc)
                 headers: '((content-type #(application/json ((charset . "utf-8")))))))

(define-syntax http-accept-case 
  (syntax-rules ()
    ((_ request (t1 e1 ...) (t2 e2 ...) ...)
     (let ((accept-headers (header-values 'accept (request-headers request))))
       (cond ((pair? (lset-intersection eq? accept-headers 't1)) e1 ...)
             ((pair? (lset-intersection eq? accept-headers 't2)) e2 ...)
             ...
             (else (send-response code: 406 reason: "Not Acceptable")))))))