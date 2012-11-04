;; Make a server for remote REPL
(use-modules (system repl server))
(spawn-server)

;;(add-to-load-path (dirname (current-filename)))
;;(display %load-path)
(primitive-load "scripts/keycodes.scm")
(primitive-load "scripts/coroutine.scm")
(primitive-load "scripts/scheduler.scm")
(primitive-load "scripts/yield.scm")
(primitive-load "scripts/bullet-types.scm")
(primitive-load "scripts/primitives.scm")
(primitive-load "scripts/player.scm")
(primitive-load "scripts/enemy.scm")

(primitive-load "scripts/spiders-nest.scm")

(define game (make-game))
(define max-bullets 10000)
(define bullets (make-bullet-system max-bullets))
(define player (make-player))
(define enemy (make-enemy 100))
(define debug-mode #f)

(define (clear-everything)
  (set-segments! agenda '())
  (clear-bullet-system! bullets))

(define (emit-arc x y radius num-bullets callback)
  (define bullet-list '())
  (let iterate ((i 0))
    (when (< i num-bullets)
      (let ((bullet (make-bullet bullets))
	    (direction (* i (/ length num-bullets))))
	(let ((pos-x (+ x (* radius (cos-deg direction))))
	      (pos-y (+ y (* radius (sin-deg direction)))))
	  (set-bullet-position! bullet pos-x pos-y)
	  (set-bullet-direction! bullet direction)
	  (set! bullet-list (cons bullet bullet-list))
	  (iterate (1+ i))))))
  (callback bullet-list))

(define (emit-spiral-forever enemy radius num-bullets rotate-step delay speed acceleration angular-velocity type)
  (coroutine
   (let repeat ((rotate 0))
     (emit-circle bullets (enemy-x enemy) (enemy-y enemy) radius num-bullets rotate speed acceleration angular-velocity type)
     (wait delay)
     (repeat (+ rotate rotate-step)))))

(define (emit-splosion enemy delay)
  (coroutine
   (let repeat ()
     (splosion-bullet (emit-circle bullets (enemy-x enemy) (enemy-y enemy) 0 6 0 100 0 0 'large-orange))
     (wait (/ delay 2))
     (splosion-bullet (emit-circle bullets (enemy-x enemy) (enemy-y enemy) 0 6 (/ 360 12) 100 0 0 'large-orange))
     (wait (/ delay 2))
     (repeat))))

(define (splosion-bullet bullet-list)
  (coroutine
   (wait 1)
   (for-each
    (lambda (bullet)
      (let loop ((i 0))
	(when (< i 8)
	  (emit-bullet bullets (bullet-x bullet) (bullet-y bullet) 120
		       (+ (bullet-direction bullet) (- (random 20) 10))
		       (random 40) (random 15) 'medium-blue)
	    (loop (1+ i))))
      (kill-bullet bullet))
    bullet-list)))

(define (player-shot)
  (coroutine
   (when (player-shooting? player)
     (let ((x (player-x player))
	   (y (player-y player))
	   (speed 800))
       (emit-bullet bullets (- x 16) y speed 260 0 0 'small-diamond)
       (emit-bullet bullets x (- y 20) speed 270 0 0 'medium-blue)
       (emit-bullet bullets (+ x 16) y speed 280 0 0 'small-diamond))
     (wait .07)
     (player-shot))))

(define (enemy-ai enemy)
  (coroutine
   (enemy-move-to enemy 0 100 200)
   (enemy-move-to enemy 800 100 200)
   (enemy-ai enemy)))

(game-on-start-hook
 game
 (lambda ()
   (define bullet-sheet (make-sprite-sheet "data/images/bullets.png" 32 32 0 0))
   (define player-sheet (make-sprite-sheet "data/images/player.png" 32 48 0 0))
   (define enemy-sheet (make-sprite-sheet "data/images/girl.png" 64 64 0 0))
   (set-bullet-system-sprite-sheet! bullets bullet-sheet)
   (set-sprite-sheet! (player-sprite player) player-sheet 0)
   (set-sprite-sheet! (enemy-sprite enemy) enemy-sheet 0)
   (set-player-position! player 400 550)
   (set-player-speed! player 350)
   (set-enemy-position! enemy 400 100)))

(game-on-update-hook
 game
 (lambda (dt)
   (update-agenda agenda dt)
   (update-bullet-system! bullets dt)
   (update-player! player dt)
   (update-enemy! enemy dt)))

(game-on-draw-hook
 game
 (lambda ()
   (draw-bullet-system bullets)
   (when debug-mode
     (draw-bullet-system-hitboxes bullets))
   (draw-player player)
   (draw-enemy enemy)))

(game-on-key-pressed-hook
 game
 (lambda (key)
   (when (eq? key (keycode 'up))
     (player-move-up! player #t))
   (when (eq? key (keycode 'down))
     (player-move-down! player #t))
   (when (eq? key (keycode 'left))
     (player-move-left! player #t))
   (when (eq? key (keycode 'right))
     (player-move-right! player #t))
   (when (eq? key (keycode 'z))
     (set-player-shooting! player #t)
     (player-shot))))

(game-on-key-released-hook
 game
 (lambda (key)
   (when (eq? key (keycode 'up))
     (player-move-up! player #f))
   (when (eq? key (keycode 'down))
     (player-move-down! player #f))
   (when (eq? key (keycode 'left))
     (player-move-left! player #f))
   (when (eq? key (keycode 'right))
     (player-move-right! player #f))
   (when (eq? key (keycode 'z))
     (set-player-shooting! player #f))
   (when (eq? key (keycode 'escape))
     (game-stop game))
  (when (eq? key (keycode 'space))
    (cave-spiders-nest bullets 400 150))))

(game-run game)
