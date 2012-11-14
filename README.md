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

    (define (player-shot)
      (coroutine
       (when (player-shooting? player)
         (let ((x (player-x player))
    	   (y (player-y player))
    	   (speed 800))
           (emit-bullet bullets (- x 16) y speed 260 0 0 'small-diamond)
           (emit-bullet bullets x (- y 20) speed 270 0 0 'medium-blue)
           (emit-bullet bullets (+ x 16) y speed 280 0 0 'small-diamond))
         (wait .07)
         (player-shot))))

This script checks if the player's shooting flag is currently true. If so, 3 bullets are fired.
The procedure then waits for .07 seconds and does it all over again until the player is no longer in the shooting state.
    
Dependencies
------------
GNU Guile >= 2.0  
Allegro >= 5.0

*Note:* Guile currently has issues compiling on Windows with MinGW. Until that is resolved, Gnumaku cannot run on Windows.

Try the Demo!
------------------
    make
    ./demo.scm

License
-------
GNU GPL v3

Thanks
------
Thanks to mark_weaver and ijp for helping me with everything Scheme and Guile related on a regular basis.
Thanks to wingo for his coroutine implementation.
Thanks to MIT Press for SICP.
Thanks to the fine folks on the #guile and #allegro rooms on freenode.