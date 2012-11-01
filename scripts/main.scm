;; Make a server for remote REPL
(use-modules (system repl server))
(spawn-server)

;;(add-to-load-path (dirname (current-filename)))
;;(display %load-path)
(primitive-load "scripts/coroutine.scm")
(primitive-load "scripts/scheduler.scm")
(primitive-load "scripts/yield.scm")
(primitive-load "scripts/primitives.scm")
(primitive-load "scripts/spiders-nest.scm")

(define game (make-game))
(define max-bullets 10000)
(define bullets (make-bullet-system max-bullets))

(define (clear-everything)
  (set-segments! agenda '())
  (clear-bullet-system! bullets))

(define (emit-arc x y radius  num-bullets callback)
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

(define (emit-spiral-forever x y rotate-step delay callback)
  (coroutine
   (let repeat ((rotate 0))
     (emit-circle bullets x y 40 8 rotate callback)
     (wait delay)
     (repeat (+ rotate rotate-step)))))

(define (emit-circle-forever x y radius num-bullets delay callback)
  (coroutine
   (let repeat ()
     (emit-circle bullets x y radius num-bullets 0 callback)
     (wait delay)
     (repeat))))

(define (emit-splosion x y delay callback)
  (coroutine
   (let repeat ()
     (emit-circle bullets x y 0 6 0 callback)
     (wait (/ delay 2))
     (emit-circle bullets x y 0 6 (/ 360 12) callback)
     (wait (/ delay 2))
     (repeat))))

(define (stress-test)
  (let iterate ((i 0))
    (when (< i max-bullets)
      (let ((bullet (make-bullet bullets)))
	(set-bullet-position!  bullet (random 800) (random 600))
	(set-bullet-direction! bullet (random 360))
	(set-bullet-speed!     bullet (+ 50 (random 50)))
	(iterate (1+ i))))))

(define (bullet-stuff bullet-list)
  (coroutine
   (for-each
    (lambda (bullet)
      (set-bullet-sprite! bullet 2)
      (set-bullet-speed! bullet 120))
    bullet-list)
   (wait 1)
   (for-each
    (lambda (bullet)
      (set-bullet-speed! bullet -60))
    bullet-list)
   (wait .75)
   (for-each
    (lambda (bullet)
      (set-bullet-speed! bullet 100)
      (set-bullet-acceleration! bullet 100)
      (set-bullet-angular-velocity! bullet 20))
    bullet-list)))

(define (cool-reverse bullet-list)
  (coroutine
   (for-each
    (lambda (bullet)
      (set-bullet-sprite! bullet 2)
      (set-bullet-speed! bullet 0))
    bullet-list)
   (wait 1)
   (for-each
    (lambda (bullet)
      (change-bullet-direction! bullet -180)
      (set-bullet-acceleration! bullet 200))
    bullet-list)))

(define (splosion-bullet bullet-list)
  (coroutine
   (for-each
    (lambda (bullet)
      (set-bullet-sprite! bullet 2)
      (set-bullet-speed! bullet 80))
    bullet-list)
   (wait 1)
   (for-each
    (lambda (bullet)
      (let loop ((i 0))
	(let ((new-bullet (make-bullet system)))
	  (when (< i 8)
	    (set-bullet-sprite! new-bullet 0)
	    (set-bullet-speed! new-bullet 120)
	    (set-bullet-position! new-bullet (bullet-x bullet) (bullet-y bullet))
	    (set-bullet-direction! new-bullet (random 360))
	    (set-bullet-angular-velocity! new-bullet (random 10))
	    (set-bullet-acceleration! new-bullet (random 40))
	    (loop (1+ i)))))
      (kill-bullet bullet))
    bullet-list)))

(define (large-bullet bullet-list)
  (for-each 
   (lambda (bullet)
     (set-bullet-sprite! bullet 2)
     (set-bullet-speed! bullet 100))
   bullet-list))

(define (diamond-bullet bullet-list)
  (for-each 
   (lambda (bullet)
     (set-bullet-sprite! bullet 1)
     (set-bullet-speed! bullet 100))
   bullet-list))

(define (chess-hell x y)
  (define bullet-list '())
  (coroutine
   (let rows ((row 0))
     (when (< row 16)
       (let ((offset (if (odd? row) 32 0)))
	 (let columns ((column 0))
	   (when (< column 8)
	     (set!
	      bullet-list
	      (cons
	       (emit-bullet
		bullets
		(+ x offset (* column 64))
		(+ y (* row 32))
		0 0 0 0 2)
	       bullet-list))
	     (wait (/ 1.0 120))
	     (columns (1+ column)))))
       (rows (1+ row))))
   (wait 1)
   (for-each
    (lambda (bullet)
      (set-bullet-speed! bullet 100)
      (set-bullet-direction! bullet (random 360)))
    bullet-list)))

(define (inward-spiral x y rotate-step delay)
  (coroutine
   (let repeat ((rotate 0)
		(radius 150))
     (when (> radius 0)
       (emit-circle x y radius 8 rotate diamond-bullet)
       (emit-circle x y radius 8 (- rotate (random 16)) diamond-bullet)
       (emit-circle x y radius 8 (- rotate (random 16)) diamond-bullet)
       (wait delay)
       (repeat (+ rotate rotate-step) (- radius 10))))))

(game-on-start-hook game (lambda ()
			   (define sprite-sheet (make-sprite-sheet "data/images/bullets.png" 32 32 0 0))
			   (set-bullet-system-sprite-sheet! bullets sprite-sheet)))

(game-on-update-hook game (lambda (dt)
			    (update-agenda agenda dt)
			    (update-bullet-system! bullets dt)))

(game-on-draw-hook game (lambda ()
			  (draw-bullet-system bullets)))

(game-run game)
