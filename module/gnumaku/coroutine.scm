(define-module (gnumaku coroutine)
  #:use-module (oop goops)
  #:export (coroutine wait))

(define (do-coroutine proc)
  "Creates a procedure that be yield and resume at any point. Used for cooperative multi-threading."
  (define (handler cont callback . args)
    (define (resume . args)
      ;; Call continuation that resumes the procedure
      (call-with-prompt 'coroutine-prompt
			(lambda () (apply cont args))
			handler))
    (when (procedure? callback)
      (apply callback resume args)))

  ;; Call procedure
  (call-with-prompt 'coroutine-prompt (lambda () (proc)) handler))

(define-syntax coroutine
  (syntax-rules ()
    ((coroutine exp ...)
     (do-coroutine (lambda () exp ...)))))

(define-generic wait)
