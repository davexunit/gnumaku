(define-module (demo enemy)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku scene-graph)
  #:use-module (demo actor)
  #:export (<enemy> sprite speed direction points health damage action))

(define-class <enemy> (<actor>)
  (sprite #:accessor sprite #:init-keyword #:sprite #:init-value #f)
  (points #:accessor points #:init-keyword #:points #:init-value 0)
  (health #:accessor health #:init-keyword #:health #:init-value 0)
  (action #:accessor action #:init-keyword #:action #:init-value (lambda () #f)))

(define-method (hitbox-size (enemy <enemy>) width height)
  (set-rect-size! (hitbox enemy) width height))

(define-method (damage (enemy <enemy>) damage)
  (set! (health enemy) (- (health enemy) damage)))

(define-method (alive? (enemy <enemy>))
  (> (health enemy) 0))

(define (run-enemy-action enemy)
  ((action enemy) enemy))

(define-method (%draw (enemy <enemy>))
  (draw-sprite (sprite enemy)))
