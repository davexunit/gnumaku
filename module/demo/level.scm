(define-module (demo level)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku director)
  #:use-module (gnumaku scheduler)
  #:use-module (gnumaku scene-graph)
  #:use-module (demo actor)
  #:use-module (demo player)
  #:use-module (demo enemy)
  #:export (<level> name width height player enemies background agenda buffer
                    player-bullet-system enemy-bullet-system
                    layer run add-enemy clear-enemies init-buffer))

(define-class <level> (<scene-node>)
  (name #:accessor name #:init-keyword #:name #:init-value "untitled")
  (width #:accessor width #:init-keyword #:width #:init-value 0)
  (height #:accessor height #:init-keyword #:height #:init-value 0)
  (player #:accessor player #:init-keyword #:player #:init-value #f)
  (enemies #:accessor enemies #:init-keyword #:enemies #:init-value '())
  (background #:accessor background #:init-keyword #:background #:init-value #f)
  (buffer #:accessor buffer #:init-keyword #:buffer #:init-value #f)
  (agenda #:accessor agenda #:init-keyword #:agenda #:init-value (make-agenda))
  (player-bullet-system #:accessor player-bullet-system #:init-keyword #:player-bullet-system #:init-value #f)
  (enemy-bullet-system #:accessor enemy-bullet-system #:init-keyword #:enemy-bullet-system #:init-value #f))

(define-method (init-buffer (level <level>))
  (set! (buffer level) (make-image (width level) (height level))))

(define-method (run (level <level>)))

(define-method (update (level <level>) dt)
  ;; Tick agenda by 1
  ;; We time things based upon number of updates, not time in seconds
  (update-agenda! (agenda level) 1)
  (update-bullet-system! (player-bullet-system level) dt)
  (update-bullet-system! (enemy-bullet-system level) dt)
  (update (player level) dt)
  (update-enemies level dt))

(define-method (update-enemies (level <level>) dt)
  (for-each (lambda (enemy) (update enemy dt)) (enemies level)))

(define-method (%draw (level <level>))
  (director-set-draw-target (buffer level))
  (draw-image (background level) 0 0)
  (draw-bullet-system (player-bullet-system level))
  (draw (player level))
  (draw-bullet-system (enemy-bullet-system level))
  (draw-enemies level)
  (director-reset-draw-target)
  (draw-image (buffer level) 0 0))

(define-method (draw-enemies (level <level>))
  (for-each (lambda (enemy) (draw enemy)) (enemies level)))

(define-method (add-enemy (level <level>) enemy)
    (set! (enemies level) (cons enemy (enemies level)))
    (set! (bullet-system enemy) (enemy-bullet-system level)))
    ;(run-enemy-action enemy))

(define-method (clear-enemies (level <level>))
  (set! (enemies level) '()))
