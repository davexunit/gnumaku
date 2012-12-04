(define-module (demo actor)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku agenda)
  #:export (<actor> level name hitbox agenda bullet-system x y get-x get-y shot-sound))

(define-class <actor> ()
  (level #:accessor level #:init-keyword #:level #:init-value #f)
  (name #:accessor name #:init-keyword #:name #:init-value "untitled")
  (x #:accessor x #:getter get-x #:init-keyword #:x #:init-value 0)
  (y #:accessor y #:getter get-y #:init-keyword #:y #:init-value 0)
  (hitbox #:accessor hitbox #:init-keyword #:hitbox #:init-form (make-rect 0 0 0 0))
  (agenda #:accessor agenda #:init-keyword #:agenda #:init-thunk make-agenda)
  (bullet-system #:accessor bullet-system #:init-keyword #:bullet-system #:init-value #f)
  (shot-sound #:accessor shot-sound #:init-keyword #:shot-sound #:init-value #f))

(define-method (update (actor <actor>) dt)
  (update-agenda (agenda actor) 1))

(define-method (draw (actor <actor>)))

(define-method (set-position (actor <actor>) new-x new-y)
  (set! (x actor) new-x)
  (set! (y actor) new-y))

(define-method (wait (actor <actor>) delay)
  (abort-to-prompt 'coroutine-prompt
                   (lambda (resume) (agenda-schedule (agenda actor) delay resume))))
