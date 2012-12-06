(define-module (gnumaku events)
  #:use-module (oop goops)
  #:export (<event-emitter> on dispatch))

(define-class <event-emitter> ()
  (events #:accessor events #:init-keyword #:events #:init-thunk make-hash-table))

(define-method (get-callbacks (emitter <event-emitter>) type)
  "Returns list of callbacks for a given event type."
  (let ((handle (hash-get-handle (events emitter) type)))
    (and handle (cdr handle))))

(define-method (add-event-callback (emitter <event-emitter>) type callback)
  "Adds a callback to the list of callbacks for an existing event type."
  (set! (events emitter) (cons callback (events emitter))))

(define-method (add-event-type (emitter <event-emitter>) type callback)
  "Creates a new event type and adds the callback to it's callback list."
  (hash-set! (events emitter) type (list callback)))

(define-method (on (emitter <event-emitter>) type callback)
  "Registers an event callback for the given event type."
  (if (get-callbacks emitter type)
      (add-event-callback emitter type callback)
      (add-event-type emitter type callback)))

(define-method (dispatch (emitter <event-emitter>) type . args)
  "Calls all callbacks for the given event type."
  (let ((callbacks (get-callbacks emitter type)))
    (when callbacks
      (for-each (lambda (callback) (apply callback args)) callbacks))))
