(define-module (gnumaku enemy)
  #:export (make-enemy enemy? enemy-sprite enemy-speed set-enemy-speed! enemy-direction
                       set-enemy-direction! enemy-points set-enemy-points! enemy-hitbox
                       enemy-health set-enemy-health! set-enemy-position! set-enemy-hitbox-size!
                       damage-enemy! enemy-alive? enemy-x enemy-y update-enemy! draw-enemy
                       enemy-move-to enemy-wait run-enemy-action enemy-bullet-system set-enemy-bullet-system!))

(use-modules (srfi srfi-9) (gnumaku core) (gnumaku scheduler))

(define-record-type Enemy
  (%make-enemy sprite action hitbox health speed direction points bullets agenda)
  enemy?
  (sprite enemy-sprite)
  (action enemy-action set-enemy-action!)
  (speed enemy-speed set-enemy-speed!)
  (direction enemy-direction set-enemy-direction!)
  (points enemy-points set-enemy-points!)
  (hitbox enemy-hitbox)
  (health enemy-health set-enemy-health!)
  (bullet-system enemy-bullet-system set-enemy-bullet-system!)
  (agenda enemy-agenda))

(define (make-enemy image action health points)
  (let ((sprite (make-sprite image)))
    (center-sprite-image! sprite)
    (%make-enemy sprite action (make-rect 0 0 0 0) health 0 0 points #f (make-agenda))))

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

(define (enemy-wait enemy delay)
  (abort-to-prompt 'coroutine-prompt
                   (lambda (resume)
                     (add-to-agenda! (enemy-agenda enemy) delay resume))))

(define (run-enemy-action enemy)
  ((enemy-action enemy) enemy))

(define (update-enemy! enemy dt)
  (update-agenda! (enemy-agenda enemy) 1)
  (set-enemy-position! enemy
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
      (enemy-wait enemy (/ (sqrt (+ (* dx dx) (* dy dy))) (enemy-speed enemy)))
      (set-enemy-speed! enemy 0))))
