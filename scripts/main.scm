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

(define (emit-circle x y radius num-bullets rotate callback)
  (define bullet-list '())
  (let iterate ((i 0))
    (when (< i num-bullets)
      (let ((bullet (make-bullet bullets)))
	(set-bullet-position bullets bullet x y)
	(set-bullet-direction bullets bullet (+ rotate (* i (/ 360 num-bullets))))
	(set! bullet-list (cons bullet bullet-list))
	(iterate (1+ i)))))
  (callback bullet-list))

(define (emit-spiral-forever x y rotate-step delay callback)
  (coroutine
   (lambda ()
     (let repeat ((rotate 0))
       (emit-circle x y 20 8 rotate callback)
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

(define (bullet-stuff bullet-list)
  (coroutine
   (lambda ()
     (for-each
      (lambda (bullet)
	(set-bullet-sprite bullets bullet 2)
	(set-bullet-speed bullets bullet 120))
      bullet-list)
     (wait 1)
     (for-each
      (lambda (bullet)
	(set-bullet-speed bullets bullet -60))
      bullet-list)
     (wait .75)
     (for-each
      (lambda (bullet)
	(set-bullet-speed bullets bullet 100)
	(set-bullet-acceleration bullets bullet 100)
	(set-bullet-angular-velocity bullets bullet 20))
      bullet-list))))

(define (large-bullet bullet-list)
  (for-each 
   (lambda (bullet)
     (set-bullet-sprite bullets bullet 2)
     (set-bullet-speed bullets bullet 100))
   bullet-list))

(define (diamond-bullet bullet-list)
  (for-each 
   (lambda (bullet)
     (set-bullet-sprite bullets bullet 1)
     (set-bullet-speed bullets bullet 100))
   bullet-list))

(define (chess-hell x y)
  (define bullet-list '())
  (coroutine
   (lambda ()
     (let rows ((row 0))
       (when (< row 16)
	 (let ((offset (if (odd? row) 32 0)))
	   (let columns ((column 0))
	     (when (< column 8)
	       (let ((bullet (make-bullet bullets)))
		 (set-bullet-position bullets bullet (+ x offset (* column 64)) (+ y (* row 32)))
		 (set-bullet-sprite bullets bullet 2)
		 (set! bullet-list (cons bullet bullet-list)))
	       (columns (1+ column)))))
	 (rows (1+ row))))
     (wait 1)
     (for-each
      (lambda (bullet)
	(set-bullet-speed bullets bullet 100)
	(set-bullet-direction bullets bullet (random 360)))
      bullet-list))))

(game-on-start-hook game (lambda ()
			   (define sprite-sheet (make-sprite-sheet "data/images/bullets.png" 32 32 0 0))
			   (set-bullet-system-sprite-sheet bullets sprite-sheet)))

(game-on-update-hook game (lambda (dt)
			    (update-agenda agenda dt)
			    (update-bullet-system bullets dt)))

(game-on-draw-hook game (lambda ()
			  (draw-bullet-system bullets)))

(game-run game)
