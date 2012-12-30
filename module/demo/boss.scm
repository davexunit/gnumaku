(define-module (demo boss)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku math)
  #:use-module (gnumaku assets)
  #:use-module (gnumaku coroutine)
  #:use-module (gnumaku scene-graph)
  #:use-module (gnumaku path)
  #:use-module (gnumaku actions)
  #:use-module (demo patterns)
  #:use-module (demo actor)
  #:use-module (demo enemy)
  #:use-module (demo level)
  #:export (<boss> make-boss))

(define (make-orbiter)
  (let ((sheet (load-asset "orbiter.png" 32 32 0 0)))
    (make-sprite (sprite-sheet-tile sheet 0) #:position (make-vector2 48 0))))

(define (make-boss-hitbox)
  (make-rect 0 0 64 32))

(define (make-boss-path boss)
  (make-bezier-path (make-vector2 0 0) (make-vector2 100 500)
                    (make-vector2 300 -50) (make-vector2 400 300)
                    (make-vector2 200 600) (make-vector2 200 -100)
                    (make-vector2 0 0)))

(define-class <boss> (<enemy>)
  (orbiter #:accessor orbiter #:init-keyword #:orbiter #:init-thunk make-orbiter)
  (path #:accessor path #:init-keyword #:path #:init-value #f))

(define (make-boss x y)
  (make <boss> #:sprite (make-sprite (sprite-sheet-tile (load-asset "enemy.png" 64 48 0 0) 0))
        #:action boss-ai #:hitbox (make-boss-hitbox) #:health 2000
        #:shot-sound (load-asset "enemy_shot.wav")))

(define-method (update (boss <boss>))
  (next-method)
  ;; Rotate orbiting thing around boss
  (let* ((orbiter (orbiter boss))
         (pos (sprite-position orbiter))
         (x (vector2-x pos))
         (y (vector2-y pos))
         (theta (/ pi 60)))
    (set-sprite-position orbiter
                         (make-vector2 (- (* x (cos theta)) (* y (sin theta)))
                                       (+ (* x (sin theta)) (* y (cos theta)))))))

(define-method (draw (boss <boss>))
    (draw-bezier-path (path boss) #:color '(1 0 0 1))
    (next-method))

(define-method (%draw (boss <boss>))
  (next-method)
  (draw-sprite (orbiter boss)))

(define-method (boss-ai (boss <boss>))
  (define-coroutine (follow-path)
    (move-bezier-path boss (path boss) '(200 200))
    (follow-path))

  (let ((w (width (level boss))))
    (set! (path boss) (make-boss-path boss)))
  (follow-path)
  (spiral2 boss))
