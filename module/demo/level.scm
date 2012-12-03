(define-module (demo level)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku director)
  #:use-module (gnumaku scheduler)
  #:use-module (gnumaku scene-graph)
  #:use-module (demo actor)
  #:use-module (demo player)
  #:use-module (demo enemy)
  #:duplicates (merge-generics replace warn last)
  #:export (<level> name width height player enemies background agenda buffer
                    player-bullet-system enemy-bullet-system scroll-speed
                    layer run add-enemy clear-enemies init-level))

(define-class <level> (<scene-node>)
  (name #:accessor name #:init-keyword #:name #:init-value "untitled")
  (width #:accessor width #:init-keyword #:width #:init-value 0)
  (height #:accessor height #:init-keyword #:height #:init-value 0)
  (player #:accessor player #:init-keyword #:player #:init-value #f)
  (enemies #:accessor enemies #:init-keyword #:enemies #:init-value '())
  (background #:accessor background #:init-keyword #:background #:init-value #f)
  (background-y #:accessor background-y #:init-keyword #:background-y #:init-value 0)
  (scroll-speed #:accessor scroll-speed #:init-keyword #:scroll-speed #:init-value 100)
  (buffer #:accessor buffer #:init-keyword #:buffer #:init-value #f)
  (agenda #:accessor agenda #:init-keyword #:agenda #:init-value (make-agenda))
  (player-bullet-system #:accessor player-bullet-system #:init-keyword #:player-bullet-system #:init-value #f)
  (enemy-bullet-system #:accessor enemy-bullet-system #:init-keyword #:enemy-bullet-system #:init-value #f))

(define-method (init-level (lvl <level>))
  (set! (buffer lvl) (make-image (width lvl) (height lvl)))
  (set! (level (player lvl)) lvl))

(define-method (run (level <level>)))

(define-method (update (level <level>) dt)
  ;; Tick agenda by 1
  ;; We time things based upon number of updates, not time in seconds
  (update-agenda! (agenda level) 1)
  (update-bullet-system! (player-bullet-system level) dt)
  (update-bullet-system! (enemy-bullet-system level) dt)
  (update (player level) dt)
  (update-enemies level dt)
  (check-player-collision level)
  (update-background level dt))

(define-method (update-background (level <level>) dt)
  (let ((y (background-y level)))
    (set! y (+ y (* (scroll-speed level) dt)))
    (when (> y (height level))
      (set! y (- y (image-height (background level)))))
    (set! (background-y level) y)))

(define-method (update-enemies (level <level>) dt)
  (for-each (lambda (enemy) (update enemy dt)) (enemies level))
  (check-enemies-collision level))

(define-method (check-player-collision (level <level>))
  (let ((hitbox (hitbox (player level)))
        (player (player level)))
    (set-rect-position! hitbox
			(- (x player) (/ (rect-width hitbox) 2))
			(- (y player) (/ (rect-height hitbox) 2)))
    (bullet-system-collide-rect (enemy-bullet-system level) hitbox (lambda () (on-player-hit level)))))

(define-method (check-enemy-collision (level <level>) enemy)
  (let ((hitbox (hitbox enemy)))
    (set-rect-position! hitbox
			(- (x enemy) (/ (rect-width hitbox) 2))
			(- (y enemy) (/ (rect-height hitbox) 2)))
    (bullet-system-collide-rect (player-bullet-system level) hitbox (lambda () (on-enemy-hit level enemy)))))

(define-method (check-enemies-collision (level <level>))
  (for-each (lambda (enemy) (check-enemy-collision level enemy)) (enemies level)))

(define-method (on-player-hit (level <level>))
  (let ((player (player level)))
    (if (invincible player)
        #f
      (begin
        ;(sprite-blink (player-sprite player) 3 30)
        (set! (lives player) (1- (lives player)))
        (invincible-mode player 180)
        ;; Return true so that the bullet that hit the player is removed
        #t))))

(define-method (on-enemy-hit (level <level>) enemy)
  (let ((player (player level)))
    (damage enemy (power player))
    (when (<= (health enemy) 0)
      (add-points player (points enemy))
      (kill-enemy level enemy))
    #t))

(define-method (kill-enemy (level <level>) enemy)
  (set! (enemies level) (delete enemy (enemies level))))

(define-method (%draw (level <level>))
  (director-set-draw-target (buffer level))
  (draw-background level)
  (draw-bullet-system (player-bullet-system level))
  (draw (player level))
  (draw-bullet-system (enemy-bullet-system level))
  (draw-enemies level)
  (director-reset-draw-target)
  (draw-image (buffer level) 0 0))

(define-method (draw-background (level <level>))
  (let ((background (background level)))
    (let scroll-pos ((y (background-y level)))
      (when (< y (height level))
        (draw-image background 0 y)
        (scroll-pos (+ y (image-height background)))))
    (let scroll-neg ((y (- (background-y level) (image-height background))))
      (when (> (+ y (image-height background)) 0)
        (draw-image background 0 y)
        (scroll-neg (- y (image-height background)))))))

(define-method (draw-enemies (level <level>))
  (for-each (lambda (enemy) (draw enemy)) (enemies level)))

(define-method (add-enemy (lvl <level>) enemy)
    (set! (enemies lvl) (cons enemy (enemies lvl)))
    (set! (bullet-system enemy) (enemy-bullet-system lvl))
    (set! (level enemy) lvl)
    ((action enemy) enemy))

(define-method (clear-enemies (level <level>))
  (set! (enemies level) '()))
