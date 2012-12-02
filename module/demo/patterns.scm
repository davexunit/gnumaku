(define-module (demo patterns)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku primitives)
  #:use-module (gnumaku actor)
  #:use-module (gnumaku coroutine)
  #:export (test-pattern-1))

(define (test-pattern-1 actor)
  (let ((x (get-x actor))
        (y (get-y actor)))
    (coroutine
     (when (bullet-system actor)
       (emit-bullet (bullet-system actor) (+ x -100 (random 200)) (+ y 50)
                    0 270 -150 0 'medium-blue))
     (wait actor 3)
     (test-pattern-1 actor))))
