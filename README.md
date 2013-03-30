Gnumaku
=======

About
-----
Gnumaku is (trying to be) a libre game engine for creating bullet hell
shooting games in the Guile programming language.

[Screencast](https://www.youtube.com/watch?v=cKRkG8I0wDU)

[Another screencast](https://www.youtube.com/watch?v=WWy8UChXAlk)

Tech
----

Gnumaku is written in Guile, with some C for performance reasons. GNU
Guile is a Scheme implementation. It is the official GNU extension
language and easy to work with.

The Gnumaku Guile modules provide a developer with the infrastructure
needed to quickly get started developing a shmup.

The Allegro 5 game library has been chosen for it's simplicity and use
of OpenGL accelerated rendering rather than software rendering. The
guile-allegro5 library provides a wrapper over the Allegro 5 C library.

A good shmup engine requires a way to make awesome bullet patterns
easily. Gnumaku features a robust bullet system capable of processing
thousands of bullets efficiently. Bullets have many customizable
properties and can be manipulated after they are emitted for creating
complex behavior.

Writing scripts using the built-in coroutines is an easy way to write
a bullet pattern in a logical, linear fashion.

Here's what a simple bullet script might look like:

_THIS CODE IS OUT OF DATE NOW_

```scheme
(define-coroutine (player-shot-1 player)
  (when (shooting player)
    (play-sample (shot-sound player) 1 0 1)
    (let ((pos (position player))
          (speed 15)
          (bullets (bullet-system player))
          (type 'sword))
      (emit-bullet bullets pos speed 269 type)
      (emit-bullet bullets pos speed 270 type)
      (emit-bullet bullets pos speed 271 type))
    (wait player 3)
    (player-shot-1 player)))
```

This script first checks if the player's shooting flag is currently
set. If so, 3 bullets are fired and a sound sample is played. The
bullets are emitted from the player's bullet system. The bullets have
a speed of 15 pixels per second. They are fired from the player's
current location with directions of 269, 270, and 271 degrees,
respectively. `'sword` refers to the type of bullet to fire. Bullet
types define the image, hitbox, and blend mode that the bullet has.
The procedure is then stopped and scheduled in the player's agenda to
resume after 3 frames have passed. When the script continues, we
recursively call the procedure and the process repeats until the
player is no longer in the shooting state.

Dependencies
------------
GNU Guile >= 2.0
Allegro >= 5.0
guile-allegro5

Platforms
---------
Gnumaku is developed primarily for the GNU/Linux operating system.

Try the Demo!
------------------
    make
    ./run-demo

License
-------
GNU GPL v3

Thanks
------
Thanks to mark_weaver and ijp for helping me with everything Scheme
and Guile related on a regular basis.

Thanks to wingo for his coroutine implementation.

Thanks to MIT Press for SICP.

Thanks to the fine folks on the #guile and #allegro rooms on freenode.