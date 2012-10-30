(define (repeat proc times)
  (let loop ((i 0))
    (when (< i times)
      (proc)
      (loop (1+ i)))))

(define (cave-spiders-nest system x y)
  (define (do-movement r delay)
    (lambda (system bullet-list)
      (coroutine
       (lambda ()
	 (for-each
	  (lambda (bullet)
	    (set-bullet-sprite! system bullet 1))
	  bullet-list)
	 (wait (- 2 delay))
	 (for-each
	  (lambda (bullet)
	    (change-bullet-direction! system bullet (- -190 r))
	    (set-bullet-acceleration! system bullet 200))
	  bullet-list)
	 (wait (- 0.66 delay))
	 (for-each
	  (lambda (bullet)
	    (set-bullet-speed! system bullet 4)
	    (change-bullet-direction! system bullet (- -190 (* r 50)))
	    (set-bullet-acceleration! system bullet 300))
	  bullet-list)))))

  (coroutine
   (lambda ()
     (let ((radius 50)
	   (angle 0)
	   (r 0)
	   (delay 0))
       (repeat
	(lambda ()
	  (emit-circle system x y radius 16 angle (do-movement r delay))
	  (set! angle (- (random 4) 2))
	  (set! radius (+ radius 8))
	  (set! r (- (random 0.3) 0.05))
	  (set! delay (+ delay (/ 1.0 60)))
	  (wait .01))
	70)))))
