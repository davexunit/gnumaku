(define (repeat proc times)
  (let loop ((i 0))
    (when (< i times)
      (proc)
      (loop (1+ i)))))

(define (do-movement system bullet-list)
  (coroutine
   (lambda ()
     (for-each
      (lambda (bullet)
	(set-bullet-killable system bullet #f)
	(set-bullet-sprite system bullet 1))
      bullet-list)
     (wait 2)
     (for-each
      (lambda (bullet)
     	(change-bullet-direction system bullet 180)
     	(set-bullet-acceleration system bullet 200))
      bullet-list)
     (wait 1)
     (for-each
      (lambda (bullet)
	(set-bullet-killable system bullet #t)
     	(set-bullet-speed system bullet 4)
     	(change-bullet-direction system bullet 180)
     	(set-bullet-acceleration system bullet 300))
      bullet-list))))

(define (cave-spiders-nest system x y)
  (coroutine
   (lambda ()
     (let ((radius 50)
	   (angle 0))
       (repeat
	(lambda ()
	  (emit-circle system x y radius 16 angle do-movement)
	  (set! angle (- (random 4) 2))
	  (set! radius (+ radius 8))
	  (wait .01))
	70)))))
