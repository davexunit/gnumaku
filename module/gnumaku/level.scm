(define-module (gnumaku level)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku scheduler)
  #:use-module (gnumaku scene-node)
  #:use-module (gnumaku actor)
  #:use-module (gnumaku player)
  #:use-module (gnumaku enemy)
  #:export (<level> name width height player enemies background agenda
                    player-bullet-system enemy-bullet-system
                    layer run add-enemy clear-enemies))

(define-class <level> ()
  (name #:accessor name #:init-keyword #:name #:init-value "untitled")
  (width #:accessor width #:init-keyword #:width #:init-value 0)
  (height #:accessor height #:init-keyword #:height #:init-value 0)
  (player #:accessor player #:init-keyword #:player #:init-value #f)
  (enemies #:accessor enemies #:init-keyword #:enemies #:init-value '())
  (background #:accessor background #:init-keyword #:background #:init-value #f)
  (agenda #:accessor agenda #:init-keyword #:agenda #:init-value (make-agenda))
  (player-bullet-system #:accessor player-bullet-system #:init-keyword #:player-bullet-system #:init-value #f)
  (enemy-bullet-system #:accessor enemy-bullet-system #:init-keyword #:enemy-bullet-system #:init-value #f)
  (layer #:accessor layer #:init-keyword #:layer #:init-value #f))

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

(define-method (draw (level <level>))
  (draw-image (background level) 0 0)
  (draw-bullet-system (player-bullet-system level))
  (draw (player level))
  (draw-bullet-system (enemy-bullet-system level))
  (draw-enemies level))

(define-method (draw-enemies (level <level>))
  (for-each (lambda (enemy) (draw enemy)) (enemies level)))

(define-method (add-enemy (level <level>) enemy)
    (set! (enemies level) (cons enemy (enemies level)))
    (set! (bullet-system enemy) (enemy-bullet-system level)))
    ;(run-enemy-action enemy))

(define-method (clear-enemies (level <level>))
  (set! (enemies level) '()))

;; (define (run-level level)
;;   ((level-on-run level) level))

;; (define (level-wait level delay)
;;   (abort-to-prompt 'coroutine-prompt
;;                    (lambda (resume)
;;                      (add-to-agenda! (level-agenda level) delay resume))))

;; (define (update-level-enemies! level dt)
;;   (for-each (lambda (enemy) (update-enemy! enemy dt)) (level-enemies level)))

;; (define (draw-level-enemies level)
;;   (for-each (lambda (enemy) (draw-enemy enemy)) (level-enemies level)))

;; (define (update-level! level dt)
;;   ;; Tick agenda by 1
;;   ;; We time things based upon number of updates, not time in seconds
;;   (update-agenda! (level-agenda level) 1)
;;   (update-bullet-system! (level-player-bullet-system level) dt)
;;   (update-bullet-system! (level-enemy-bullet-system level) dt)
;;   (update-player! (level-player level) dt)
;;   (update-level-enemies! level dt))

;; (define (draw-player level)
;;   (let ((player (level-player level)))
;;     (draw-sprite (player-sprite player))))

;; (define (draw-level level)
;;   (draw-sprite (level-background level))
;;   (draw-bullet-system (level-player-bullet-system level))
;;   (draw-player level)
;;   (draw-bullet-system (level-enemy-bullet-system level))
;;   (draw-level-enemies level))
