;; Core C library plus any other useful miscellaneous procedures.

(define-module (gnumaku core)
  #:export (repeat make-sprite))

(load-extension "./gnumaku.so" "init_gnumaku")

(define (repeat times proc)
  "Do something a bunch of times."
  (do ((i 1 (1+ i)))
       ((> i times))
    (proc i)))

(define* (make-sprite image #:optional #:key (position (make-vector2 0 0))
                      (scale (make-vector2 1 1)) (rotation 0)
                      (color (make-color-f 1 1 1 1)) (anchor #f))
  "Helpful wrapper around %make-sprite primitive procedure."
  (%make-sprite image position scale rotation color anchor))
