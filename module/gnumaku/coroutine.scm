(define-module (gnumaku coroutine)
  #:use-module (oop goops)
  #:export (define-coroutine wait))

(define (do-coroutine proc)
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
  (call-with-prompt 'coroutine-prompt proc handler))

;; Creates a procedure that is executed as a coroutine.
(define-syntax define-coroutine
  (syntax-rules ()
    ((_ (name ...) . body)
     (define (name ...) (do-coroutine (lambda () . body))))))

;; Generic method definition for yielding from a coroutine for an amount of time.
;; This is done here to avoid module import issues that can arise when a generic
;; is defined in more than once place.
(define-generic wait)
