(define-module (gnumaku bullet-types)
  #:export (set-bullet-type register-bullet-type))
(use-modules (gnumaku core))

(define bullet-types (make-hash-table))

(define (register-bullet-type key proc)
  (hash-set! bullet-types key proc))

(define (set-bullet-type bullet key)
  ((hash-ref bullet-types key) bullet))
