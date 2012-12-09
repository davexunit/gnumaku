(define-module (demo patterns)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku primitives)
  #:use-module (gnumaku coroutine)
  #:use-module (gnumaku scene-graph)
  #:use-module (demo actor)
  #:use-module (demo level)
  #:export (test-pattern-1 spiral-1 fire-at-player))

(define (test-pattern-1 actor)
  (let ((x (x actor))
        (y (y actor)))
    (coroutine
     (when (bullet-system actor)
       (emit-bullet (bullet-system actor) (+ x -100 (random 200)) (+ y 50)
                    0 90 5 0 'small-diamond))
     (wait actor 3)
     (test-pattern-1 actor))))

(define (spiral-1 actor)
  (coroutine
   (let loop ((rotate 0))
     (let ((x (x actor))
           (y (y actor)))
       (when (shot-sound actor)
         (play-sample (shot-sound actor) 1.0 0.0 1.0))
       (emit-circle (bullet-system actor) x y 24 10 (+ rotate (random 10)) 2 0 .2 'small-diamond)
       (wait actor 15)
       (loop (+ rotate (/ 360 40)))))))

(define (fire-at-player actor)
  (let ((player (player (level actor))))
    (coroutine
     (let loop ()
       (when (shot-sound actor)
         (play-sample (shot-sound actor) 1.0 0.0 1.0))
       (emit-towards (bullet-system actor) (x actor) (y actor) 4
                     (- (x player) 80) (y player) 0 -.2 'small-diamond)
       (emit-towards (bullet-system actor) (x actor) (y actor) 4
                     (x player) (y player) 0 0 'small-diamond)
       (emit-towards (bullet-system actor) (x actor) (y actor) 4
                     (+ (x player) 80) (y player) 0 .2 'small-diamond)
       (wait actor 45)
       (loop)))
    (coroutine
     (let fire-circle ()
       (emit-circle (bullet-system actor) (x actor) (y actor) 48 32 0 2.5 0 0 'sword)
       (wait actor 60)
       (fire-circle)))))
