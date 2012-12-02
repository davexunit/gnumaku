(define-module (demo actor)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku scheduler)
  #:export (<actor> name agenda bullet-system x y get-x get-y))

(define-class <actor> ()
  (name #:accessor name #:init-keyword #:name #:init-value "untitled")
  (x #:accessor x #:getter get-x #:init-keyword #:x #:init-value 0)
  (y #:accessor y #:getter get-y #:init-keyword #:y #:init-value 0)
  (agenda #:accessor agenda #:init-keyword #:agenda #:init-thunk make-agenda)
  (bullet-system #:accessor bullet-system #:init-keyword #:bullet-system #:init-value #f))

(define-method (update (actor <actor>) dt))

(define-method (draw (actor <actor>)))

(define-method (set-position (actor <actor>) new-x new-y)
  (set! (x actor) new-x)
  (set! (y actor) new-y))

(define-method (wait (actor <actor>) delay)
  (abort-to-prompt 'coroutine-prompt
                   (lambda (resume) (add-to-agenda! (agenda actor) delay resume))))
