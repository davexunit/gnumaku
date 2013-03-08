#! /usr/bin/guile -s
!#

;; Just a simple bootstrap script.
(add-to-load-path ".")
(use-modules (system repl server)
             (gnumaku director)
             (gnumaku scenes shooter))

;; Create REPL server for live-coding awesomeness.
(spawn-server)

;; Start game.
(director-init)
(director-run (make-shooter-scene))
