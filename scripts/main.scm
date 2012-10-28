;; Make a server for remote REPL
(use-modules (system repl server))
(spawn-server)

;;(add-to-load-path (dirname (current-filename)))
;;(display %load-path)
(primitive-load "scripts/coroutine.scm")
(primitive-load "scripts/scheduler.scm")

(define game (make-game))
(define max-bullets 10000)
(define bullets (make-bullet-system max-bullets))
(define agenda (make-agenda))

(define (wait delay)
  (abort-to-prompt 'coroutine-prompt (lambda (resume)
				       (add-to-agenda! agenda delay resume))))

(define (clear-everything)
  (set-segments! agenda '())
  (clear-bullet-system bullets))

(define (emit-circle x y num-bullets rotate callback)
  (define bullet-list '())
  (let iterate ((i 0))
    (when (< i num-bullets)
      (let ((bullet (make-bullet bullets)))
	(set-bullet-position bullets bullet x y)
	(set-bullet-direction bullets bullet (+ rotate (* i (/ 360 num-bullets))))
	(set! bullet-list (cons bullet bullet-list))
	(iterate (1+ i)))))
  (callback bullet-list))

(define (bullet-stuff bullet-list)
  (coroutine
   (lambda ()
     (for-each
      (lambda (bullet)
	(set-bullet-speed bullets bullet 120))
	;;(set-bullet-angular-velocity bullets bullet 180))
      bullet-list)
     (wait 1)
     (for-each
      (lambda (bullet)
	(set-bullet-speed bullets bullet 0))
      bullet-list)
     (wait 1)
     (for-each
      (lambda (bullet)
	(set-bullet-speed bullets bullet 100)
	(set-bullet-angular-velocity bullets bullet 0))
      bullet-list))))

(define (emit-spiral-forever x y rotate-step delay)
  (coroutine
   (lambda ()
     (let repeat ((rotate 0))
       (emit-circle x y 5 rotate bullet-stuff)
       (wait delay)
       (repeat (+ rotate rotate-step))))))

(define (stress-test)
  (let iterate ((i 0))
    (when (< i max-bullets)
      (let ((bullet (make-bullet bullets)))
	(set-bullet-position  bullets bullet (random 800) (random 600))
	(set-bullet-direction bullets bullet (random 360))
	(set-bullet-speed     bullets bullet (+ 50 (random 50)))
	(iterate (1+ i))))))

(game-on-start-hook game (lambda ()
			   (set-bullet-system-image bullets "data/images/bullet.png")))

(game-on-update-hook game (lambda (dt)
			    (update-agenda agenda dt)
			    (update-bullet-system bullets dt)))

(game-on-draw-hook game (lambda ()
			  (draw-bullet-system bullets)))

(game-run game)
