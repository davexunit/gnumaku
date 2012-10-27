;; Make a server for remote REPL
(use-modules (system repl server))
(spawn-server)

(add-to-load-path (dirname (current-filename)))
(display %load-path)
(load "coroutine.scm")
(load "scheduler.scm")

(define game (make-game))
(define max-bullets 10000)
(define bullets (make-bullet-system max-bullets))
(define agenda (make-agenda))

(define (wait delay)
  (abort-to-prompt 'coroutine-prompt (lambda (resume)
				       (add-to-agenda! agenda delay resume))))

(define (emit-circle x y num-bullets speed rotate)
  (let iterate ((i 0))
    (if (< i num-bullets)
	(begin
	  (let ((bullet (make-bullet bullets)))
	    (set-bullet-position  bullets bullet x y)
	    (set-bullet-direction bullets bullet (+ (* i (/ 360 num-bullets)) rotate))
	    (set-bullet-speed     bullets bullet speed)
	    (iterate (1+ i)))))))

(define (emit-spiral-forever x y rotate-step delay)
  (coroutine
   (lambda ()
     (let repeat ((rotate 0))
       (emit-circle x y 4 90 rotate)
       (wait delay)
       (repeat (+ rotate rotate-step))))))

;; (define (emit-weird-thing times)
;;   (let ((x (random 800))
;; 	(y (random 600)))
;;     (let iterate ((i 0))
;;       (if (< i times)
;; 	  (begin
;; 	    (let ((bullet (make-bullet bullets)))
;; 	      (set-bullet-position bullets bullet (+ x (random 20)) (+ y (random 20)))
;; 	      (set-bullet-direction bullets bullet (random 360))
;; 	      (fork (lambda ()
;; 		      (wait 1)
;; 		      (set-bullet-speed bullets bullet 130)))
;; 	      (iterate (1+ i))))))))

(game-on-start-hook game (lambda ()
			   (set-bullet-system-image bullets "bullet.png")
			   (emit-spiral-forever 400 300 -22.5 .2)))

(game-on-update-hook game (lambda (dt)
			    (update-agenda agenda dt)
			    (update-bullet-system bullets dt)))

(game-on-draw-hook game (lambda ()
			  (draw-bullet-system bullets)))

(game-run game)
