(define-module (demo spiders-nest)
  #:export (cave-spiders-nest))

(use-modules (gnumaku core) (gnumaku primitives) (gnumaku coroutine) (gnumaku yield))

(define (repeat proc times)
  (let loop ((i 0))
    (when (< i times)
      (proc)
      (loop (1+ i)))))

(define (cave-spiders-nest system x y)
  (define (do-movement bullet-list r delay)
    (coroutine
     (for-each
      (lambda (bullet)
        (set-bullet-sprite! bullet 1))
      bullet-list)
     (wait (- 2 delay))
     (for-each
      (lambda (bullet)
        (change-bullet-direction! bullet (- -190 r))
        (set-bullet-acceleration! bullet 200))
      bullet-list)
     (wait (- 0.66 delay))
     (for-each
      (lambda (bullet)
        (set-bullet-speed! bullet 4)
        (change-bullet-direction! bullet (- -190 (* r 50)))
        (set-bullet-acceleration! bullet 300))
      bullet-list)))

  (coroutine
   (let ((radius 50)
	 (angle 0)
	 (r 0)
	 (delay 0))
     (repeat
      (lambda ()
	(do-movement (emit-circle system x y radius 16 angle 0 0 0 'small-diamond) r delay)
	(set! angle (- (random 4) 2))
	(set! radius (+ radius 8))
	(set! r (- (random 0.3) 0.05))
	(set! delay (+ delay (/ 1.0 60)))
	(wait .01))
      70))))
