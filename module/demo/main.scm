(define-module (demo main)
  #:use-module (oop goops)
  #:use-module (system repl server)
  #:use-module (gnumaku core)
  #:use-module (gnumaku assets)
  #:use-module (gnumaku director)
  #:use-module (gnumaku bullet-types)
  #:use-module (demo scenes shmup)
  #:export (main))

;; Seed random number generator
(set! *random-state* (random-state-from-platform))

;; Make a server for remote REPL
(spawn-server)

(define (init-bullet-types)
  (register-bullet-type 'medium-blue 
                        (lambda (bullet)
                          (set-bullet-sprite! bullet 0)
                          (set-bullet-hitbox! bullet -3 -3 6 6)))

  (register-bullet-type 'small-diamond
                        (lambda (bullet)
                          (set-bullet-sprite! bullet 1)
                          (set-bullet-hitbox! bullet -2 -2 4 4)))

  (register-bullet-type 'large-orange
                        (lambda (bullet)
                          (set-bullet-sprite! bullet 2)
                          (set-bullet-hitbox! bullet -5 -5 10 10)))

  (register-bullet-type 'sword
                        (lambda (bullet)
                          (set-bullet-sprite! bullet 3)
                          (set-bullet-hitbox! bullet -16 -6 32 12)))

  (register-bullet-type 'small-green
                        (lambda (bullet)
                          (set-bullet-sprite! bullet 5)
                          (set-bullet-hitbox! bullet -2 -2 4 4))))

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
