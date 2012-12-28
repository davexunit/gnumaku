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
(define-coroutine (player-shot-1 player)
  (when (shooting player)
    (play-sample (shot-sound player) 1.0 0.0 1.0)
    (let ((x (x player))
          (y (y player))
          (speed 15)
          (bullets (bullet-system player)))
      (emit-simple-bullet bullets x y speed 269 'sword)
      (emit-simple-bullet bullets x y speed 270 'sword)
      (emit-simple-bullet bullets x y speed 271 'sword))
    (wait player 3)
    (player-shot-1 player)))
```

This script first checks if the player's shooting flag is currently set.
If so, 3 bullets are fired and a sound sample is played.
The bullets are emitted from the player's bullet system.
The bullets have a speed of 15 pixels per second.
They are fired from the player's current location with directions of 269, 270,
and 271 degrees,respectively.
`'sword` refers to the type of bullet to fire. Bullet types define the image,
hitbox, and blend mode that the bullet has.
The procedure is then stopped and scheduled in the player's agenda  to resume
after 3 frames have passed.
When the script continues, we recursively call the procedure and the process
repeats until the player is no longer in the shooting state.
    
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