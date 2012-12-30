(define-module (demo actor)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku coroutine)
  #:use-module (gnumaku core)
  #:use-module (gnumaku scene-graph)
  #:use-module (gnumaku agenda)
  #:use-module (gnumaku events)
  #:export (<actor> level name hitbox agenda bullet-system shot-sound))

(define-class <actor> (<scene-node> <event-emitter>)
  (level #:accessor level #:init-keyword #:level #:init-value #f)
  (name #:accessor name #:init-keyword #:name #:init-value "untitled")
  (hitbox #:accessor hitbox #:init-keyword #:hitbox #:init-form (make-rect 0 0 0 0))
  (agenda #:accessor agenda #:init-keyword #:agenda #:init-thunk make-agenda)
  (bullet-system #:accessor bullet-system #:init-keyword #:bullet-system #:init-value #f)
  (shot-sound #:accessor shot-sound #:init-keyword #:shot-sound #:init-value #f))

(define-method (update (actor <actor>))
  (update-agenda (agenda actor) 1))

(define-method (wait (actor <actor>) delay)
  (abort-to-prompt 'coroutine-prompt
                   (lambda (resume) (agenda-schedule (agenda actor) delay resume))))
