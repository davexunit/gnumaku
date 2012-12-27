(define-module (gnumaku bullet-types)
  #:use-module (srfi srfi-9)
  #:use-module (gnumaku core)
  #:export (get-bullet-type register-bullet-type))

(define bullet-types (make-hash-table))

(define (register-bullet-type key type)
  "Associates a key with a procedure for setting particular bullet properties."
  (hash-set! bullet-types key type))

(define (get-bullet-type key)
  "Applies the bullet properties associated with the given key."
  (hash-ref bullet-types key))
