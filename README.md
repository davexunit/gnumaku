Gnumaku
=======

About
-----
Gnumaku is (trying to be) a libre bullet hell shooting game. It is not currently nowhere close to being a game yet.

Tech
----
Gnumaku is written in C and Scheme. GNU Guile is the Scheme implementation used, as it is the official GNU extension language and very easy to use.
The Allegro 5 game library has been chosen for it's simplicity and use of OpenGL accelerated rendering rather than software rendering.

Through the power of Guile, it is possible to write very awesome bullet patterns easily.
Using a simple implementation of coroutines it is very easy to write scripts that seem to execute concurrently, but without the problems of multithreading.
This sample code creates a 4-armed spiral bullet emitter:

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

Dependencies
------------
GNU Guile >= 2.0
Allegro >= 5.0

License
-------
GNU GPL v3

Thanks
------
In particular, thanks to mark_weaver for helping me with everything Scheme and Guile related on a regular basis.
Thanks to wingo for his coroutine implementation.
Thanks to MIT Press for SICP.
Many thanks to the fine folks on the #guile and #allegro rooms on freenode.