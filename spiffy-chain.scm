(module spiffy-chain

(chain-handlers)

(import chicken scheme)
(use srfi-1)

(define (chain-handlers . wrappers)
  (let ((wrappers (reverse wrappers)))
    (fold (lambda (wrapper handler)
            (lambda args
              (wrapper (lambda ()
                         (apply handler args)))))
          (car wrappers)
          (cdr wrappers))))

)