(define (coroutine proc)
  (define (handler cont callback . args)
    (define (resume . args)
      (call-with-prompt 'coroutine-prompt
			(lambda () (apply cont args))
			handler))
    (when (procedure? callback)
      (apply callback resume args)))

  (call-with-prompt 'coroutine-prompt
		    (lambda () (proc))
		    handler))
