(define-module (demo boss)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku math)
  #:use-module (gnumaku assets)
  #:use-module (gnumaku primitives)
  #:use-module (gnumaku coroutine)
  #:use-module (gnumaku scene-graph)
  #:use-module (demo patterns)
  #:use-module (demo actor)
  #:use-module (demo enemy)
  #:export (<boss> make-boss))

(define (make-orbiter)
  (let ((sprite (make-sprite (sprite-sheet-tile (load-asset "orbiter.png" 32 32 0 0) 0))))
    (set-sprite-position! sprite 48 0)
    (center-sprite-image! sprite)
    sprite))

(define (make-boss-hitbox)
  (make-rect 0 0 64 32))

(define-class <boss> (<enemy>)
  (orbiter #:accessor orbiter #:init-keyword #:orbiter #:init-thunk make-orbiter))

(define (make-boss x y)
  (let ((sprite (make-sprite (sprite-sheet-tile (load-asset "enemy.png" 64 48 0 0) 0))))
    (center-sprite-image! sprite)
    (make <boss> #:x x #:y y #:sprite sprite #:action boss-ai
          #:hitbox (make-boss-hitbox) #:health 2000
          #:shot-sound (load-asset "enemy_shot.wav"))))

(define-method (update (boss <boss>) dt)
  (next-method)
  ;; Rotate orbiting thing around boss
  (let ((orbiter (orbiter boss)))
    (let ((x (sprite-x orbiter))
          (y (sprite-y orbiter))
          (theta (/ pi 60)))
      (set-sprite-position! orbiter
                            (- (* x (cos theta)) (* y (sin theta)))
                            (+ (* x (sin theta)) (* y (cos theta)))))))

(define-method (%draw (boss <boss>))
  (next-method)
  (draw-sprite (orbiter boss)))

(define-method (boss-ai (boss <boss>))
  (spiral-1 boss))
