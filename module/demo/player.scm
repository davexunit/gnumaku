(define-module (demo player)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku coroutine)
  #:use-module (gnumaku scheduler)
  #:use-module (demo actor)
  #:export (<player> make-player sprite speed movement shooting score lives strength hitbox
                     invincible bounds shot shot-sound set-movement))

(define (make-movement-hash)
  (let ((hash (make-hash-table)))
    (hash-set! hash 'up #f)
    (hash-set! hash 'down #f)
    (hash-set! hash 'left #f)
    (hash-set! hash 'right #f)
    hash))

(define-class <player> (<actor>)
  (sprite #:accessor sprite #:init-keyword #:sprite #:init-value #f)
  (speed #:accessor speed #:init-keyword #:speed #:init-value 300)
  (movement #:accessor movement #:init-keyword #:movement #:init-thunk make-movement-hash)
  (s #:accessor s #:init-keyword #:s #:init-value #f)
  (shooting #:accessor shooting #:init-keyword #:shooting #:init-value #f
            #:allocation #:virtual
            #:slot-ref (lambda (player)
                         (slot-ref player 's))
            #:slot-set! (lambda (player new-shooting)
                          (slot-set! player 's new-shooting)
                          ;; SHOOT!
                          (when (and new-shooting (procedure? (slot-ref player 'shot)))
                            ((shot player) player))))
  (score #:accessor score #:init-keyword #:score #:init-value 0)
  (lives #:accessor lives #:init-keyword #:lives #:init-value 3)
  (strength #:accessor strength #:init-keyword #:strength #:init-value 10)
  (hitbox #:accessor hitbox #:init-keyword #:hitbox #:init-form (make-rect 0 0 6 6))
  (invincible #:accessor invincible #:init-keyword #:invincible #:init-value #f)
  (bounds #:accessor bounds #:init-keyword #:bounds #:init-form (make-rect 0 0 800 600))
  (shot #:accessor shot #:init-keyword #:shot #:init-value #f)
  (shot-sound #:accessor shot-sound #:init-keyword #:shot-sound #:init-value #f))

(define (make-player bounds image)
  (let ((player (make <player> #:bounds bounds #:sprite (make-sprite image))))
    (center-sprite-image! (sprite player))
    player))

(define-method (draw (player <player>))
  (draw-sprite (sprite player)))

(define-method (update (player <player>) dt)
  (update-agenda! (agenda player) 1)
  (when (moving? player)
    (let ((direction (direction player)))
      (set! (x player) (+ (x player) (dx player direction dt)))
      (set! (y player) (+ (y player) (dy player direction dt)))
      (restrict-bounds player)))
  (set-sprite-position! (sprite player) (x player) (y player)))

(define-method (dx (player <player>) direction dt)
  (* (speed player) (cos direction) dt))

(define-method (dy (player <player>) direction dt)
  (* (speed player) (sin direction) dt))

(define-method (set-movement (player <player>) direction flag)
  (hash-set! (movement player) direction flag))

(define-method (direction? (player <player>) direction)
  (hash-ref (movement player) direction))

(define-method (moving? (player <player>))
  (or
   (direction? player 'up)
   (direction? player 'down)
   (direction? player 'left)
   (direction? player 'right)))

(define-method (direction (player <player>))
  (let ((x 0)
	(y 0))
    (when (direction? player 'left)
      (set! x (- x 1)))
    (when (direction? player 'right)
      (set! x (+ x 1)))
    (when (direction? player 'up)
      (set! y (- y 1)))
    (when (direction? player 'down)
      (set! y (+ y 1)))
    (atan y x)))

(define-method (restrict-bounds (player <player>))
  (let ((bounds (bounds player)))
    (let ((x (x player))
          (y (y player))
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
      (set-position player x y))))

;; (define (set-player-hitbox-size! player width height)
;;   (let ((hitbox (player-hitbox player)))
;;     (set-rect-size! hitbox width height)))

;; (define (player-dec-lives! player)
;;   (set-player-lives! player (1- (player-lives player))))

;; (define (player-add-points! player points)
;;   (set-player-score! player (+ (player-score player) points)))

;; (define (player-wait player delay)

;; (define (player-invincible-mode! player duration)
;;   (coroutine
;;    (set-player-invincible! player #t)
;;    (player-wait player duration)
;;    (set-player-invincible! player #f)))


;; (define (player-x player)
;;   (sprite-x (player-sprite player)))

;; (define (player-y player)
;;   (sprite-y (player-sprite player)))

