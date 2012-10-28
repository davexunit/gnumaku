Gnumaku
=======

About
-----
Gnumaku is (trying to be) a libre bullet hell shooting game. It is currently a young project with very little functionality.

Tech
----
Gnumaku is written in C and Scheme. GNU Guile is the Scheme implementation used, as it is the official GNU extension language and very easy to use.
The Allegro 5 game library has been chosen for it's simplicity and use of OpenGL accelerated rendering rather than software rendering.

Through the power of Guile, it is possible to write very awesome bullet patterns easily.
Using a simple implementation of coroutines it is very easy to write scripts that seem to execute concurrently, but without the problems of multithreading.
Here's what a bullet script might look like:

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

Dependencies
------------
GNU Guile >= 2.0  
Allegro >= 5.0

License
-------
GNU GPL v3

Thanks
------
Thanks to mark_weaver for helping me with everything Scheme and Guile related on a regular basis.
Thanks to wingo for his coroutine implementation.
Thanks to MIT Press for SICP.
Thanks to the fine folks on the #guile and #allegro rooms on freenode.