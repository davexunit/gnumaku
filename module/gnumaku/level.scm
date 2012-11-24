(define-module (gnumaku level)
  #:export (make-level level? run-level level-wait update-level! draw-level level-on-run
                       level-background level-agenda level-player-bullets level-enemy-bullets
                       level-layer))

(use-modules (srfi srfi-9) (gnumaku core) (gnumaku scheduler) (gnumaku player) (gnumaku layer))

(define-record-type Level
  (%make-level width height on-run player background agenda player-bullets enemy-bullets layer)
  level?
  (width level-width)
  (height level-height)
  (on-run level-on-run) ;; Procedure
  (player level-player)
  (background level-background) ;; Sprite
  (agenda level-agenda) ;; Agenda
  (player-bullets level-player-bullets) ;; Bullet system
  (enemy-bullets level-enemy-bullets)
  (layer level-layer))

(define (make-level width height on-run player background-image)
  (let ((level (%make-level width height on-run player (make-sprite background-image) (make-agenda)
               (make-bullet-system 2000) (make-bullet-system 10000)
               (make-layer (make-rect 0 0 width height) #f))))
    (set-player-bullets! player (level-player-bullets level))
    (set-layer-draw-proc! (level-layer level) (lambda () (draw-level level)))
    (set-layer-clip! (level-layer level) #t)
    level))

(define (run-level level)
  ((level-on-run level) level))

(define (level-wait level delay)
  (abort-to-prompt 'coroutine-prompt
                   (lambda (resume)
                     (add-to-agenda! (level-agenda level) delay resume))))

(define (update-level! level dt)
  ;; Tick agenda by 1
  ;; We time things based upon number of updates, not time in seconds
  (update-agenda! (level-agenda level) 1)
  (update-bullet-system! (level-player-bullets level) dt)
  (update-bullet-system! (level-enemy-bullets level) dt)
  (update-player! (level-player level) dt))

(define (draw-player level)
  (let ((player (level-player level)))
    (draw-sprite (player-sprite player))))

(define (draw-level level)
  (draw-sprite (level-background level))
  (draw-bullet-system (level-player-bullets level))
  (draw-player level)
  (draw-bullet-system (level-enemy-bullets level)))

