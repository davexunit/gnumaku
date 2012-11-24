(define-module (demo levels demo)
  #:export (demo-level))

(use-modules (gnumaku core) (gnumaku level) (gnumaku primitives) (gnumaku coroutine) (demo enemies))

(define (demo-level level)
  #f)

(define (emit-test level)
  (coroutine
   (let loop ((system (level-enemy-bullet-system level))
              (rotate 0))
     (emit-circle system (/ (level-width level) 2) 100 30 12 (* -1 rotate) 100 0 -10 'large-orange)
     (level-wait level 15)
     (emit-circle system (/ (level-width level) 2) 100 30 12 rotate 100 0 10 'medium-blue)
     (level-wait level 15)
     (loop system (+ rotate 5)))))

