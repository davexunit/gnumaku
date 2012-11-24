(define-module (demo levels demo)
  #:export (demo-level))

(use-modules (gnumaku core) (gnumaku level) (gnumaku primitives) (gnumaku coroutine))

(define (demo-level level)
  (emit-test level))

(define (emit-test level)
  (coroutine
   (let ((system (level-enemy-bullets level)))
     (emit-circle system 200 200 30 12 0 100 0 0 'medium-blue)
     (level-wait level 2)
     (emit-circle system 200 200 30 12 0 100 0 0 'medium-blue))))
