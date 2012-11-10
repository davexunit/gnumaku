(define-module (demo enemy)
  #:export (make-enemy enemy? enemy-sprite enemy-speed set-enemy-speed! enemy-direction
                       set-enemy-direction! enemy-points set-enemy-points! enemy-hitbox
                       enemy-health set-enemy-health! set-enemy-position! set-enemy-hitbox-size!
                       damage-enemy! enemy-alive? enemy-x enemy-y update-enemy! draw-enemy
                       enemy-move-to))
(use-modules (srfi srfi-9) (gnumaku core) (gnumaku yield))

(define-record-type Enemy
  (%make-enemy sprite hitbox health speed direction points)
  enemy?
  (sprite enemy-sprite)
  (speed enemy-speed set-enemy-speed!)
  (direction enemy-direction set-enemy-direction!)
  (points enemy-points set-enemy-points!)
  (hitbox enemy-hitbox)
  (health enemy-health set-enemy-health!))

(define (make-enemy health points)
  (%make-enemy (make-sprite) (make-rect 0 0 0 0) health 0 0 points))

(define (set-enemy-position! enemy x y)
  (set-sprite-position! (enemy-sprite enemy) x y))

(define (set-enemy-hitbox-size! enemy width height)
  (let ((hitbox (enemy-hitbox enemy)))
    (set-rect-size! hitbox width height)))

(define (damage-enemy! enemy damage)
  (set-enemy-health! enemy (- (enemy-health enemy) damage)))

(define (enemy-alive? enemy)
  (> (enemy-health enemy) 0))

(define (enemy-x enemy)
  (sprite-x (enemy-sprite enemy)))

(define (enemy-y enemy)
  (sprite-y (enemy-sprite enemy)))

(define (%enemy-dx enemy dt)
  (* (enemy-speed enemy) (cos (enemy-direction enemy)) dt))

(define (%enemy-dy enemy dt)
  (* (enemy-speed enemy) (sin  (enemy-direction enemy)) dt))

(define (update-enemy! enemy dt)
  (set-enemy-position!
   enemy
   (+ (enemy-x enemy) (%enemy-dx enemy dt))
   (+ (enemy-y enemy) (%enemy-dy enemy dt))))

(define (draw-enemy enemy)
  (draw-sprite (enemy-sprite enemy)))

(define (enemy-move-to enemy x y speed)
  (unless (eq? speed 0)
    (let ((dx (- x (enemy-x enemy)))
	  (dy (- y (enemy-y enemy))))
      (set-enemy-direction! enemy (atan dy dx))
      (set-enemy-speed! enemy speed)
      (wait (/ (sqrt (+ (* dx dx) (* dy dy))) (enemy-speed enemy)))
      (set-enemy-speed! enemy 0))))
