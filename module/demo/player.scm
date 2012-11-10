(define-module (demo player)
  #:export (make-player player? player-sprite player-speed set-player-speed!
                        player-moving-up? player-moving-down? player-moving-left?
                        player-moving-right? player-moving? player-move-up! player-move-down! 
                        player-move-left! player-move-right! player-shooting? set-player-shooting!
                        player-score set-player-score! player-lives set-player-lives!
                        player-hitbox player-strength player-invincible? set-player-invincible!
                        set-player-position! set-player-hitbox-size! player-dec-lives!
                        player-add-points! player-invincible-mode! player-x player-y
                        player-direction update-player! draw-player))
(use-modules (srfi srfi-9) (gnumaku core) (gnumaku coroutine) (gnumaku yield))

(define-record-type Player
  (%make-player sprite hitbox strength speed score lives invincible)
  player?
  (sprite player-sprite)
  (speed player-speed set-player-speed!)
  (move-left player-moving-left? player-move-left!)
  (move-right player-moving-right? player-move-right!)
  (move-up player-moving-up? player-move-up!)
  (move-down player-moving-down? player-move-down!)
  (shooting player-shooting? set-player-shooting!)
  (score player-score set-player-score!)
  (lives player-lives set-player-lives!)
  (hitbox player-hitbox)
  (strength player-strength)
  (invincible player-invincible? set-player-invincible!))

(define (make-player lives strength speed)
  (%make-player (make-sprite) (make-rect 0 0 6 6) strength speed 0 lives #f))

(define (set-player-position! player x y)
  (set-sprite-position! (player-sprite player) x y))

(define (set-player-hitbox-size! player width height)
  (let ((hitbox (player-hitbox player)))
    (set-rect-size! hitbox width height)))

(define (player-dec-lives! player)
  (set-player-lives! player (1- (player-lives player))))

(define (player-add-points! player points)
  (set-player-score! player (+ (player-score player) points)))

(define (player-invincible-mode! player duration)
  (coroutine
   (set-player-invincible! player #t)
   (wait duration)
   (set-player-invincible! player #f)))

(define (player-x player)
  (sprite-x (player-sprite player)))

(define (player-y player)
  (sprite-y (player-sprite player)))

(define (%player-dx player direction dt)
  (* (player-speed player) (cos direction) dt))

(define (%player-dy player direction dt)
  (* (player-speed player) (sin  direction) dt))

(define (player-moving? player)
  (or
   (player-moving-left?  player)
   (player-moving-right? player)
   (player-moving-up?    player)
   (player-moving-down?  player)))

(define (player-direction player)
  (let ((x 0)
	(y 0))
    (when (player-moving-left? player)
      (set! x (- x 1)))
    (when (player-moving-right? player)
      (set! x (+ x 1)))
    (when (player-moving-up? player)
      (set! y (- y 1)))
    (when (player-moving-down? player)
      (set! y (+ y 1)))
    (atan y x)))

(define (update-player! player dt)
  (when (player-moving? player)
    (let ((direction (player-direction player)))
      (set-player-position!
       player
       (+ (player-x player) (%player-dx player direction dt))
       (+ (player-y player) (%player-dy player direction dt))))))

(define (draw-player player)
  (draw-sprite (player-sprite player)))
