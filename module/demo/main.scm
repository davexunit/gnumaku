
(define-module (demo main)
  #:use-module (oop goops)
  #:use-module (system repl server)
  #:use-module (gnumaku core)
  #:use-module (gnumaku assets)
  #:use-module (gnumaku director)
  #:use-module (gnumaku bullet)
  #:use-module (demo scenes shmup)
  #:export (main))

;; Seed random number generator
(set! *random-state* (random-state-from-platform))

;; Make a server for remote REPL
(spawn-server)

(define (init-bullet-types)
  (register-bullet-type 'medium-blue
                        (make-bullet-type 0 (make-rect -4 -4 8 8) 'add #f))

  (register-bullet-type 'small-diamond
                        (make-bullet-type 1 (make-rect -2 -2 4 4) 'alpha #t))

  (register-bullet-type 'large-orange
                        (make-bullet-type 2 (make-rect -5 -5 10 10) 'alpha #f))

  (register-bullet-type 'sword
                        (make-bullet-type 3 (make-rect -4 -4 8 8) 'alpha #t))
  
  (register-bullet-type 'bright
                        (make-bullet-type 4 (make-rect -8 -8 16 16) 'add #f))

  (register-bullet-type 'small-green
                        (make-bullet-type 5 (make-rect -2 -2 4 4) 'alpha #f)))

(define (init-assets-manager)
  (register-asset-manager "images" load-image)
  (register-asset-manager "sprite_sheets" load-sprite-sheet)
  (register-asset-manager "sounds" load-sample)
  (register-asset-manager "fonts" load-font))

(define (main)
  (init-assets-manager)
  (init-bullet-types)
  ;; Start up director
  (director-init 800 600)
  (set! director-show-fps #t)
  (director-run (make <shmup-scene>)))
