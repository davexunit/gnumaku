Gnumaku
=======

About
-----
Gnumaku is (trying to be) a libre bullet hell shmup engine that allows you to create awesome bullet patterns using Scheme.

Tech
----
Gnumaku is written in C and Scheme. GNU Guile is the Scheme implementation used, as it is the official GNU extension language and very easy to use.
The Allegro 5 game library has been chosen for it's simplicity and use of OpenGL accelerated rendering rather than software rendering.

Through the power of Guile, it is possible to write very awesome bullet patterns easily.
Using coroutines it is very easy to write scripts that seem to execute concurrently, but without the problems of multithreading.
Here's what a bullet script might look like:

```scheme
(coroutine
 (let loop ()
   (when (shooting player)
     (play-sample (shot-sound player) 1.0 0.0 1.0)
     (let ((x (x player))
           (y (y player))
           (speed 15)
           (bullets (bullet-system player)))
       (emit-bullet bullets (- x 16) y speed 269 0 0 'sword)
       (emit-bullet bullets x (- y 20) speed 270 0 0 'sword)
       (emit-bullet bullets (+ x 16) y speed 271 0 0 'sword))
     (wait player 3)
     (loop)))))
```

This script first checks if the player's shooting flag is currently true.
If so, 3 bullets are fired and a sound sample is played.
Then the procedure is stopped and scheduled to resume 3 frames from now.
Upon re-entering the script, we repeat this process until the player is no longer in the shooting state.
    
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