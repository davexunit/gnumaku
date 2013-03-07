(define-module (gnumaku coroutine)
  #:export (coroutine
            define-coroutine))

(define (do-coroutine thunk)
  "Creates a procedure that be yield and resume at any point. Used for cooperative multi-threading."
  (define (handler cont callback . args)
    (define (resume . args)
      ;; Call continuation that resumes the procedure.
      (call-with-prompt 'coroutine-prompt
			(lambda () (apply cont args))
			handler))
    (when (procedure? callback)
      (apply callback resume args)))

  ;; Call procedure.
  (call-with-prompt 'coroutine-prompt thunk handler))

(define-syntax coroutine
  (syntax-rules ()
    ((_ (. args) . body)
     (lambda (. args)
       (do-coroutine (lambda () . body))))))

;; Creates a procedure that is executed as a coroutine.
(define-syntax define-coroutine
  (syntax-rules ()
    ((_ (name . args) . body)
     ;; Outer define for the do-coroutine call.
     (define (name . args)
       ;; Make a new procedure with the same signature so that a recursive procedure
       ;; can be created without starting a new coroutine with each call to itself.
       (define (name . args) . body)
       ;; Coroutine time.
       (do-coroutine (lambda () (name . args)))))))
