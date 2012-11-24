(define-module (gnumaku player)
  #:export (make-player player? player-sprite player-speed set-player-speed!
                        player-moving-up? player-moving-down? player-moving-left?
                        player-moving-right? player-moving? player-move-up! player-move-down! 
                        player-move-left! player-move-right! player-shooting? set-player-shooting!
                        player-score set-player-score! player-lives set-player-lives!
                        player-hitbox player-strength player-invincible? set-player-invincible!
                        set-player-bounds! set-player-position! set-player-hitbox-size! player-dec-lives!
                        player-shot set-player-shot! player-bullet-system set-player-bullet-system!
                        player-add-points! player-invincible-mode! player-x player-y
                        player-direction update-player! draw-player player-wait))
(use-modules (srfi srfi-9) (gnumaku core) (gnumaku coroutine) (gnumaku scheduler))

(define-record-type Player
  (%make-player sprite bounds hitbox strength speed score lives invincible agenda bullet-system)
  player?
  (sprite player-sprite)
  (speed player-speed set-player-speed!)
  (move-left player-moving-left? player-move-left!)
  (move-right player-moving-right? player-move-right!)
  (move-up player-moving-up? player-move-up!)
  (move-down player-moving-down? player-move-down!)
  (shooting player-shooting? %set-player-shooting!)
  (score player-score set-player-score!)
  (lives player-lives set-player-lives!)
  (hitbox player-hitbox)
  (strength player-strength)
  (invincible player-invincible? set-player-invincible!)
  (bounds player-bounds set-player-bounds!)
  (agenda player-agenda)
  (shot player-shot set-player-shot!)
  (bullet-system player-bullet-system set-player-bullet-system!))

(define (make-player image lives strength speed)
  (let ((sprite (make-sprite image)))
    (center-sprite-image! sprite)
    (%make-player sprite (make-rect 0 0 800 600) (make-rect 0 0 6 6) strength speed 0 lives #f (make-agenda) #f)))

(define (set-player-position! player x y)
  (set-sprite-position! (player-sprite player) x y))

(define (set-player-hitbox-size! player width height)
  (let ((hitbox (player-hitbox player)))
    (set-rect-size! hitbox width height)))

(define (player-dec-lives! player)
  (set-player-lives! player (1- (player-lives player))))

(define (player-add-points! player points)
  (set-player-score! player (+ (player-score player) points)))

(define (player-wait player delay)
  (abort-to-prompt 'coroutine-prompt
                   (lambda (resume)
                     (add-to-agenda! (player-agenda player) delay resume))))

(define (player-invincible-mode! player duration)
  (coroutine
   (set-player-invincible! player #t)
   (player-wait player duration)
   (set-player-invincible! player #f)))

(define (set-player-shooting! player shooting)
  (%set-player-shooting! player shooting)
  ;; SHOOT!
  (when (and shooting (procedure? (player-shot player)))
   ((player-shot player) player)))

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

(define (restrict-player-bounds player)
  (let ((bounds (player-bounds player)))
    (let ((x (player-x player))
          (y (player-y player))
          (min-x (rect-x bounds))
          (min-y (rect-y bounds))
          (max-x (+ (rect-x bounds) (rect-width bounds)))
          (max-y (+ (rect-y bounds) (rect-height bounds))))
      ;; Confine x and y to within the boundaries so the player doesn't scroll off screen
      (when (< x min-x)
        (set! x min-x))
      (when (< y min-y)
        (set! y min-y))
      (when (> x max-x)
        (set! x max-x))
      (when (> y max-y)
        (set! y max-y))
      ;; Update position
      (set-player-position! player x y))))

(define (update-player! player dt)
  (update-agenda! (player-agenda player) 1)
  (when (player-moving? player)
    (let ((direction (player-direction player)))
      (set-player-position!
       player
       (+ (player-x player) (%player-dx player direction dt))
       (+ (player-y player) (%player-dy player direction dt)))
      (restrict-player-bounds player))))

(define (draw-player player)
  (draw-sprite (player-sprite player)))
