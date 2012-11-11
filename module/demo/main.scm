(define-module (demo main))
(export main)

(load-extension "./gnumaku.so" "init_gnumaku_module")
(use-modules (system repl server) (gnumaku core) (gnumaku fps) (gnumaku coroutine) (gnumaku keycodes)
             (gnumaku scheduler) (gnumaku yield) (gnumaku primitives) (gnumaku bullet-types)
             (demo player) (demo enemy) (demo spiders-nest))

;; Make a server for remote REPL
(spawn-server)

(define game (make-game))
(define max-bullets 10000)
(define enemy-bullets (make-bullet-system max-bullets))
(define player-bullets (make-bullet-system 2000))
(define player (make-player 3 10 350))
(define enemies '())
(define debug-mode #f)
(define bullet-sheet #f)
(define player-sheet #f)
(define enemy-sheet  #f)
(define font #f)
(define fps (make-fps game))

(define (clear-everything)
  (clear-agenda! wait-scheduler)
  (set! enemies '())
  (clear-bullet-system! enemy-bullets)
  (clear-bullet-system! player-bullets))

(define (emit-spiral-forever enemy radius num-bullets rotate-step delay speed acceleration angular-velocity type)
  (coroutine
   (let repeat ((rotate 0))
     (when (enemy-alive? enemy)
       (change-type (emit-circle enemy-bullets (enemy-x enemy) (enemy-y enemy) radius num-bullets rotate speed acceleration angular-velocity type))
       (wait delay)
       (repeat (+ rotate rotate-step))))))

(define (change-type bullets)
  (coroutine
   (wait 1)
   (for-each (lambda (bullet) (set-bullet-type! bullet 'medium-blue)(set-bullet-acceleration! bullet 20)) bullets)
   (wait 1)
   (for-each (lambda (bullet) (set-bullet-type! bullet 'large-orange)) bullets)))

(define (emit-splosion enemy delay)
  (coroutine
   (let repeat ()
     (when (enemy-alive? enemy)
       (splosion-bullet (emit-circle enemy-bullets (enemy-x enemy) (enemy-y enemy) 0 6 0 100 0 0 'large-orange))
       (wait (/ delay 2))
       (splosion-bullet (emit-circle enemy-bullets (enemy-x enemy) (enemy-y enemy) 0 6 (/ 360 12) 100 0 0 'large-orange))
       (wait (/ delay 2))
       (repeat)))))

(define (splosion-bullet bullet-list)
  (coroutine
   (wait 1)
   (for-each
    (lambda (bullet)
      (let loop ((i 0))
	(when (< i 8)
	  (emit-bullet enemy-bullets (bullet-x bullet) (bullet-y bullet) 120
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
       (emit-bullet player-bullets (- x 16) y speed 268 0 0 'small-diamond)
       (emit-bullet player-bullets x (- y 20) speed 270 0 0 'small-green)
       (emit-bullet player-bullets (+ x 16) y speed 272 0 0 'small-diamond))
     (wait .07)
     (player-shot))))

(define (enemy-ai enemy)
  (set-enemy-position! enemy 400 150)
  (coroutine
   (let loop ()
     (enemy-move-to enemy 300 150 100)
     (enemy-move-to enemy 500 150 100)
     (loop))))

(define (sprite-blink sprite duration times)
  (let ((delay (/ duration times)))
    (coroutine
     (let loop ((i 0)
		(visible #f))
       (when (< i times)
	 (set-sprite-visible! sprite visible)
	 (wait delay)
	 (loop (1+ i) (not visible))))))
  (set-sprite-visible! sprite #t))

(define (check-player-collision)
  (let ((hitbox (player-hitbox player)))
    (set-rect-position! hitbox
			(- (player-x player) (/ (rect-width hitbox) 2))
			(- (player-y player) (/ (rect-height hitbox) 2)))
    (bullet-system-collide-rect enemy-bullets hitbox on-player-hit)))

(define (check-enemy-collision enemy)
  (let ((hitbox (enemy-hitbox enemy)))
    (set-rect-position! hitbox
			(- (enemy-x enemy) (/ (rect-width hitbox) 2))
			(- (enemy-y enemy) (/ (rect-height hitbox) 2)))
    (bullet-system-collide-rect player-bullets hitbox (on-enemy-hit enemy))))

(define (check-enemies-collision)
  (let loop ((enemies enemies))
    (unless (null? enemies)
      (check-enemy-collision (car enemies))
      (loop (cdr enemies)))))

(define (on-player-hit)
  (unless (player-invincible? player)
    (sprite-blink (player-sprite player) 3 30)
    (player-dec-lives! player)
    (player-invincible-mode! player 3))
  ;; Return true so that the bullet that hit the player is removed
  #t)

(define (on-enemy-hit enemy)
  (lambda ()
    (damage-enemy! enemy (player-strength player))
    (when (<= (enemy-health enemy) 0)
      (player-add-points! player (enemy-points enemy))
      (set! enemies (delete enemy enemies))
      (coroutine
       (wait 2)
       (add-test-enemy)))
    #t))

(define (add-enemy! enemy)
  (set! enemies (cons enemy enemies)))

(define (update-enemies! dt)
  (let loop ((enemies enemies))
    (unless (null? enemies)
      (update-enemy! (car enemies) dt)
      (loop (cdr enemies)))))

(define (draw-enemies)
  (let loop ((enemies enemies))
    (unless (null? enemies)
      (draw-enemy (car enemies))
      (loop (cdr enemies)))))

(define (add-test-enemy)
  (let ((enemy (make-enemy 30 100)))
    (set-sprite-sheet! (enemy-sprite enemy) enemy-sheet 0)
    (set-enemy-position! enemy (random 800) (random 200))
    (set-enemy-hitbox-size! enemy 32 32)
    (enemy-ai enemy)23
    (emit-spiral-forever enemy 32 4 8 .07 120 10 5 'small-diamond)
    (add-enemy! enemy)))

(game-on-start-hook
 game
 (lambda ()
   (set! bullet-sheet (make-sprite-sheet "data/images/bullets.png" 32 32 0 0))
   (set! player-sheet (make-sprite-sheet "data/images/player.png" 32 48 0 0))
   (set! enemy-sheet (make-sprite-sheet "data/images/girl.png" 64 64 0 0))
   (set! font (make-font "data/fonts/CarroisGothic-Regular.ttf" 18))
   (set-bullet-system-sprite-sheet! enemy-bullets bullet-sheet)
   (set-bullet-system-sprite-sheet! player-bullets bullet-sheet)
   (set-sprite-sheet! (player-sprite player) player-sheet 0)
   (set-player-position! player 400 550)
   (add-test-enemy)))

(game-on-update-hook
 game
 (lambda (dt)
   (update-fps! fps)
   (update-agenda! wait-scheduler dt)
   (update-bullet-system! enemy-bullets dt)
   (update-bullet-system! player-bullets dt)
   (update-player! player dt)
   (update-enemies! dt)
   (check-player-collision)
   (check-enemies-collision)))

(game-on-draw-hook
 game
 (lambda ()
   (draw-bullet-system player-bullets)
   (draw-bullet-system enemy-bullets)
   (when debug-mode
     (draw-bullet-system-hitboxes player-bullets)
     (draw-bullet-system-hitboxes enemy-bullets))
   (draw-player player)
   (draw-enemies)
   (font-draw-text font 10 10 '(1 1 1 1) (string-append "Lives: " (number->string (player-lives player))))
   (font-draw-text font 100 10 '(1 1 1 1) (string-append "Score: " (number->string (player-score player))))
   (font-draw-text font 730 575 '(1 1 1 0.7) (string-append "FPS: " (number->string (fps-last-frames fps))))))

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
   (when (eq? key (keycode 'd))
     (set! debug-mode (not debug-mode)))))

(define (main)
  (game-run game))
