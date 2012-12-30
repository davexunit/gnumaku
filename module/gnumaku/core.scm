;; Core C library plus any other useful miscellaneous procedures.

(define-module (gnumaku core)
  #:export (repeat make-sprite))

(load-extension "./gnumaku.so" "init_gnumaku")

(define (repeat times proc)
  "Do something a bunch of times."
  (do ((i 1 (1+ i)))
       ((> i times))
    (proc i)))
