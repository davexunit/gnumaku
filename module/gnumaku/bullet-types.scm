(define-module (gnumaku bullet-types)
  #:use-module (gnumaku core)
  #:export (set-bullet-type register-bullet-type))

(define bullet-types (make-hash-table))

(define (register-bullet-type key proc)
  "Associates a key with a procedure for setting particular bullet properties."
  (hash-set! bullet-types key proc))

(define (set-bullet-type bullet key)
  "Applies the bullet properties associated with the given key."
  ((hash-ref bullet-types key) bullet))
