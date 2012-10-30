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

    (define (emit-circle-forever x y radius num-bullets delay callback)
      (coroutine
       (let repeat ()
         (emit-circle bullets x y radius num-bullets 0 callback)
         (wait delay)
         (repeat))))
    
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