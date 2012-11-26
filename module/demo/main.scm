(define-module (demo main)
  #:use-module (oop goops)
  #:use-module (system repl server)
  #:use-module (gnumaku core)
  #:use-module (gnumaku director)
  #:use-module (demo scenes shmup)
  #:export (main))

;; Seed random number generator
(set! *random-state* (random-state-from-platform))

;; Make a server for remote REPL
(spawn-server)

(define (main)
  ;; Start up director
  (director-init 800 600)
  (set! director-show-fps #t)
  (director-run (make <shmup-scene>)))
