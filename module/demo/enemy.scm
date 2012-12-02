(define-module (demo enemy)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku scheduler)
  #:use-module (demo actor)
  #:export (<enemy> sprite hitbox speed direction points health))

(define-class <enemy> (<actor>)
  (sprite #:accessor sprite #:init-keyword #:sprite #:init-value #f)
  (hitbox #:accessor hitbox #:init-keyword #:hitbox #:init-form (make-rect 0 0 0 0))
  (speed #:accessor speed #:init-keyword #:speed #:init-value 0)
  (direction #:accessor direction #:init-keyword #:direction #:init-value 0)
  (points #:accessor points #:init-keyword #:points #:init-value 0)
  (health #:accessor health #:init-keyword #:health #:init-value 0))

(define-method (hitbox-size (enemy <enemy>) width height)
  (set-rect-size! (hitbox enemy) width height))

(define-method (damage (enemy <enemy>) damage)
  (set! (health enemy) (- (health enemy) damage)))

(define-method (alive? (enemy <enemy>))
  (> (enemy-health enemy) 0))

(define-method (dx (enemy <enemy>) dt)
  (* (speed enemy) (cos (direction enemy)) dt))

(define-method (dy (enemy <enemy>) dt)
  (* (speed enemy) (sin  (direction enemy)) dt))

(define (run-enemy-action enemy)
  ((enemy-action enemy) enemy))

(define-method (update (enemy <enemy>) dt)
  (update-agenda! (agenda enemy) 1)
  (set-position enemy
                (+ (x enemy) (dx enemy dt))
                (+ (y enemy) (dy enemy dt)))
  (set-sprite-position! (sprite enemy) (x enemy) (y enemy)))

(define-method (draw (enemy <enemy>))
  (draw-sprite (sprite enemy)))

(define (enemy-move-to enemy x y speed)
  (unless (eq? speed 0)
    (let ((dx (- x (enemy-x enemy)))
	  (dy (- y (enemy-y enemy))))
      (set-enemy-direction! enemy (atan dy dx))
      (set-enemy-speed! enemy speed)
      (enemy-wait enemy (/ (sqrt (+ (* dx dx) (* dy dy))) (enemy-speed enemy)))
      (set-enemy-speed! enemy 0))))
