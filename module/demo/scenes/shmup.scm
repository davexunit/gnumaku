(define-module (demo scenes shmup)
  #:use-module (oop goops)
  #:use-module (gnumaku core)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku director)
  #:use-module (gnumaku scene)
  #:use-module (gnumaku keycodes)
  #:use-module (gnumaku coroutine)
  #:use-module (gnumaku primitives)
  #:use-module (gnumaku scene-node)
  #:use-module (gnumaku level)
  #:use-module (gnumaku actor)
  #:use-module (gnumaku player)
  #:use-module (gnumaku enemy)
  #:use-module (demo enemies)
  #:use-module (demo hud)
  #:use-module (demo levels demo)
  #:duplicates (merge-generics)
  #:export (<shmup-scene> background))

(define-class <shmup-scene> (<scene>)
  (background #:accessor background #:init-keyword #:background #:init-value #f)
  (field-width #:accessor field-width #:init-keyword #:field-width #:init-value 480)
  (field-height #:accessor field-height #:init-keyword #:field-height #:init-value 560)
  (player #:accessor player #:init-keyword #:player #:init-value #f)
  (hud #:accessor hud #:init-keyword #:hud #:init-value #f)
  (player-sheet #:accessor player-sheet #:init-keyword #:player-sheet #:init-value #f)
  (enemy-sheet #:accessor enemy-sheet #:init-keyword #:enemy-sheet #:init-value #f)
  (current-level #:accessor current-level #:init-keyword #:current-level #:init-value #f))

(define-method (draw (scene <shmup-scene>))
  (draw-image (background scene) 0 0)
  ;(draw-layer (layer (current-level scene)))
  (draw (current-level scene))
  (draw-hud (hud scene)))

(define-method (update (scene <shmup-scene>) dt)
  (update (current-level scene) dt))

(define-method (on-start (scene <shmup-scene>))
  (load-assets scene)
  (init-player scene)
  (set! (hud scene) (make-hud 800 600 (player scene)))
  (set! (current-level scene) (make-demo-level (player scene) (field-width scene) (field-height scene)))
  (set-position (current-level scene) 20 20)
  (run (current-level scene)))

(define-method (load-assets (scene <shmup-scene>))
  (set! (player-sheet scene) (make-sprite-sheet "data/images/player.png" 32 48 0 0))
  (set! (enemy-sheet scene) (make-sprite-sheet "data/images/girl.png" 64 64 0 0))
  (set! (background scene) (load-image "data/images/background.png")))

;; (define-method (load-level (scene <shmup-scene>))
;;   (let ((level (make <level> #:width (field-width scene) #:height (field-height scene)
;;                            demo-level (player scene) (load-image "data/images/space.png"))))
;;     (set-bullet-system-sprite-sheet! (level-enemy-bullet-system level) (bullet-sheet scene))
;;     (set-bullet-system-sprite-sheet! (level-player-bullet-system level) (bullet-sheet scene))
;;     (set-layer-position! (level-layer level) 20 20)
;;     (set! (current-level scene) level)))

(define-method (init-player (scene <shmup-scene>))
  (set! (player scene) (make-player (make-rect 0 0 (field-width scene) (field-height scene))
                                    (sprite-sheet-tile (player-sheet scene) 0)))
  (set! (shot (player scene)) player-shot-1)
  (set-position (player scene) (/ (field-width scene) 2) (- (field-height scene) 32)))

(define (player-shot-1 player)
  (coroutine
   (when (shooting player)
     (let ((x (x player))
	   (y (y player))
	   (speed 800)
           (bullets (bullet-system player)))
       (emit-bullet bullets (- x 16) y speed 270 0 0 'small-diamond)
       (emit-bullet bullets x (- y 20) speed 270 0 0 'small-diamond)
       (emit-bullet bullets (+ x 16) y speed 270 0 0 'small-diamond))
     (wait player 6)
     (player-shot-1 player))))

(define-method (add-test-enemy (scene <shmup-scene>))
  (let ((enemy (make-enemy-1 (random (field-width scene)) (random 150)
                             (sprite-sheet-tile (enemy-sheet scene) 0))))
    (add-enemy (current-level scene) enemy)))

(define-method (on-key-pressed (scene <shmup-scene>) key)
  (when (eq? key (keycode 'up))
    (set-movement (player scene) 'up #t))
  (when (eq? key (keycode 'down))
    (set-movement (player scene) 'down #t))
  (when (eq? key (keycode 'left))      
    (set-movement (player scene) 'left #t))
  (when (eq? key (keycode 'right))     
    (set-movement (player scene) 'right #t))
   (when (eq? key (keycode 'z))
     (set! (shooting (player scene)) #t)))

(define-method (on-key-released (scene <shmup-scene>) key)
  (when (eq? key (keycode 'escape))
    (director-pop-scene))
   (when (eq? key (keycode 'up))
     (set-movement (player scene) 'up #f))
   (when (eq? key (keycode 'down))      
     (set-movement (player scene) 'down #f))
   (when (eq? key (keycode 'left))      
     (set-movement (player scene) 'left #f))
   (when (eq? key (keycode 'right))     
     (set-movement (player scene) 'right #f))
   (when (eq? key (keycode 'z))
     (set! (shooting (player scene)) #f))
   (when (eq? key (keycode 'w))
     (clear-enemies (current-level scene)))
   (when (eq? key (keycode 'q))
     (add-test-enemy scene)))
