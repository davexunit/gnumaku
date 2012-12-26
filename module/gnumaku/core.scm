;; Core C library plus any other useful miscellaneous procedures.

(define-module (gnumaku core)
  #:export (repeat))

(load-extension "./gnumaku.so" "init_gnumaku")

(define (repeat times proc)
  (do ((i 1 (1+ i)))
       ((> i times))
    (proc i)))
