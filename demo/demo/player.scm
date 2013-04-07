(define-module (demo player)
  #:use-module (oop goops)
  #:use-module (srfi srfi-9)
  #:use-module (gnumaku sprite)
  #:use-module (gnumaku vector)
  #:export (<player>
            make-player
            player?
            player-sprite
            set-player-sprite!
            player-position
            set-player-position!
            player-speed
            set-player-speed!
            player-movement
            set-player-movement!
            player-shooting?
            set-player-shooting!
            player-score
            set-player-score!
            player-credits
            set-player-credits!
            player-lives
            set-player-lives!
            player-invincible?
            set-player-invincible!
            player-bounds
            set-player-bounds!
            player-hitbox
            set-player-hitbox!
            update-player!
            draw-player
            set-player-direction!
            player-direction?
            player-moving?
            player-angle
            player-add-points!))

(define-record-type <player>
  (%make-player sprite position speed movement shooting score credits lives
                invincible bounds hitbox)
  player?
  (sprite player-sprite set-player-sprite!)
  (position player-position set-player-position!)
  (speed player-speed set-player-speed!)
  (movement player-movement set-player-movement!)
  (shooting player-shooting? set-player-shooting!)
  (score player-score set-player-score!)
  (credits player-credits set-player-credits!)
  (lives player-lives set-player-lives!)
  (invincible player-invincible? set-player-invincible!)
  (bounds player-bounds set-player-bounds!)
  (hitbox player-hitbox set-player-hitbox!))

(define (make-player sprite position speed credits lives bounds hitbox)
  (let ((movement '((up . #f)
                    (down . #f)
                    (left . #f)
                    (right . #f))))
    (%make-player sprite position speed movement #f 0 credits lives #f bounds hitbox)))

(define (draw-player player)
  (draw-sprite (player-sprite player)))

(define (update-player! player)
  (set-player-position! player
                         (vadd (player-position player)
                               (player-velocity player)))
  (set-sprite-position! (player-sprite player) (player-position player)))

(define (set-player-direction! player direction flag)
  (assoc-set! (player-movement player) direction flag))

(define (player-direction? player direction)
  (assoc-ref (player-movement player) direction))

(define (player-moving? player)
  (or (player-direction? player 'up)
      (player-direction? player 'down)
      (player-direction? player 'left)
      (player-direction? player 'right)))

(define (player-angle player)
  (let ((x 0)
	(y 0))
    (when (player-direction? player 'left)
      (set! x (- x 1)))
    (when (player-direction? player 'right)
      (set! x (+ x 1)))
    (when (player-direction? player 'up)
      (set! y (- y 1)))
    (when (player-direction? player 'down)
      (set! y (+ y 1)))
    (atan y x)))

(define (player-direction-vector player)
  (define (boolean->int bool)
    (if bool 1 0))

  (vnormalize
   (list (- (boolean->int (player-direction? player 'right))
            (boolean->int (player-direction? player 'left)))
         (- (boolean->int (player-direction? player 'down))
            (boolean->int (player-direction? player 'up))))))

(define (player-velocity player)
  (vscale (player-speed player)
          (player-direction-vector player)))

;; (define (restrict-player-bounds! player)
;;   (let* ((pos (position player))
;;         (x (vector2-x pos))
;;         (y (vector2-y pos))
;;         (bounds (rect-move (bounds player) pos))
;;         (left (rect-x bounds))
;;         (top (rect-y bounds))
;;         (right (+ (rect-x bounds) (rect-width bounds)))
;;         (bottom (+ (rect-y bounds) (rect-height bounds)))
;;         (width (width level))
;;         (height (height level)))
;;     ;; Confine x and y to within the boundaries so the player doesn't scroll off screen
;;     (when (< left 0)
;;       (set! x (- x left)))
;;     (when (< top 0)
;;       (set! y (- y top)))
;;     (when (> right width)
;;       (set! x (- x (- right width))))
;;     (when (> bottom height)
;;       (set! y (- y (- bottom height))))
;;     ;; Update position
;;     (set! (position player) (make-vector2 x y))))

(define (player-add-points! player points)
  (set-player-score! player (+ (player-score player) points)))
