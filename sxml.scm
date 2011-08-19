(define (string-blank? s)
  (irregex-match '(* space) s))

(define (simple-format str)
  (let loop ((input (with-input-from-string str read-lines)))
    (receive (lines rest)
        (break string-blank? (drop-while string-blank? input))
      (if (null? lines)
          '()
          (cons `(p ,(intersperse lines '(br)))
                (loop rest))))))

(define (sxml-page->html page)
  (with-output-to-string 
      (lambda ()
        (SRV:send-reply
         (pre-post-order* `((inject "<!DOCTYPE html>")
                            (html ,page))
                          `((inject *preorder* . ,(lambda (_ x) x))
                            . ,universal-conversion-rules*))))))
