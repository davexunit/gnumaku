(define-module (demo level)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku director)
  #:use-module (gnumaku agenda)
  #:use-module (gnumaku scene-graph)
  #:use-module (gnumaku actions)
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
  (scroll-speed #:accessor scroll-speed #:init-keyword #:scroll-speed #:init-value 1)
  (buffer #:accessor buffer #:init-keyword #:buffer #:init-value #f)
  (agenda #:accessor agenda #:init-keyword #:agenda #:init-value (make-agenda))
  (player-bullet-system #:accessor player-bullet-system #:init-keyword #:player-bullet-system #:init-value #f)
  (enemy-bullet-system #:accessor enemy-bullet-system #:init-keyword #:enemy-bullet-system #:init-value #f))

(define-method (init-level (lvl <level>))
  (set! (buffer lvl) (make-image (width lvl) (height lvl)))
  (set! (level (player lvl)) lvl)
  (set-bounds lvl (player-bullet-system lvl))
  (set-bounds lvl (enemy-bullet-system lvl)))

(define-method (set-bounds (lvl <level>) system)
  (let ((padding 100))
    (set-bullet-system-bounds system
                              (make-rect (* -1 padding) (* -1 padding)
                                         (+ (* 2 padding) (width lvl))
                                         (+ (* 2 padding) (height lvl))))))

(define-method (run (level <level>)))

(define-method (update (level <level>))
  ;; Tick agenda by 1
  ;; We time things based upon number of updates, not time in seconds
  (update-agenda (agenda level) 1)
  (update-bullet-system (player-bullet-system level))
  (update-bullet-system (enemy-bullet-system level))
  (update (player level))
  (update-enemies level)
  (check-player-collision level)
  (update-background level))

(define-method (update-background (level <level>))
  (let ((y (background-y level)))
    (set! y (+ y (scroll-speed level)))
    (when (> y (height level))
      (set! y (- y (image-height (background level)))))
    (set! (background-y level) y)))

(define-method (update-enemies (level <level>))
  (for-each (lambda (enemy) (update enemy)) (enemies level))
  (check-enemies-collision level))

(define-method (check-player-collision (level <level>))
  (let ((player (player level)))
        (let* ((pos (position player))
               (hitbox (rect-move (hitbox player) pos))
               (graze-hitbox (rect-move (graze-hitbox player) pos)))
          (bullet-system-collide-rect (enemy-bullet-system level) graze-hitbox
                                      (lambda (bullet)
                                        (on-player-graze level bullet)))
          (bullet-system-collide-rect (enemy-bullet-system level) hitbox
                                      (lambda (bullet)
                                        (on-player-hit level bullet))))))

(define-method (check-enemy-collision (level <level>) enemy)
  (let* ((pos (position enemy))
        (hitbox (rect-move (hitbox enemy) pos)))
    (bullet-system-collide-rect (player-bullet-system level) hitbox
                                (lambda (bullet)
                                  (on-enemy-hit level enemy bullet)))))

(define-method (check-enemies-collision (level <level>))
  (for-each (lambda (enemy) (check-enemy-collision level enemy)) (enemies level)))

(define-method (on-player-graze (level <level>) bullet)
  (let ((player (player level)))
    (set! (graze-count player) (1+ (graze-count player))))
  #f)

(define-method (on-player-hit (level <level>) bullet)
  (let ((player (player level)))
    (if (invincible player)
        #f
      (begin
        ;(sprite-blink (player-sprite player) 180 30)
        (set! (lives player) (1- (lives player)))
        (invincible-mode player 180)
        ;; Return true so that the bullet that hit the player is removed
        #t))))

(define-method (on-enemy-hit (level <level>) enemy bullet)
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
  ;(draw-bullet-system-hitboxes (enemy-bullet-system level))
  (draw-enemies level)
  (director-reset-render-image)
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
