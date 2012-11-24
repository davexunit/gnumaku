(define-module (demo levels demo)
  #:export (demo-level))

(use-modules (gnumaku core) (gnumaku level) (gnumaku primitives) (gnumaku coroutine))

(define (demo-level level)
  (emit-test level))

(define (emit-test level)
  (coroutine
   (let loop ((system (level-enemy-bullets level))
              (rotate 0))
     (emit-circle system 300 200 30 12 (* -1 rotate) 100 0 -10 'large-orange)
     (level-wait level 15)
     (emit-circle system 300 200 30 12 rotate 100 0 10 'medium-blue)
     (level-wait level 15)
     (loop system (+ rotate 5)))))

