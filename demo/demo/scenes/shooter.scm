(define-module (demo scenes shooter)
  #:use-module (oop goops)
  #:use-module (allegro keyboard)
  #:use-module (allegro graphics)
  #:use-module (gnumaku scene)
  #:use-module (gnumaku director)
  #:use-module (gnumaku sprite)
  #:use-module (gnumaku player)
  #:export (make-shooter-scene))

(define bitmap #f)

(define (make-default-player)
  (make-player #f '(320 460) 4 1 3 #f #f))

;; (define (make-player-bullets)
;;   (make-particle-system 2000))

(define-class <shooter-scene> ()
  (player #:accessor player #:init-keyword #:player #:init-thunk make-default-player)
  ;(player-bullets #:accessor player-bullets #:init-keyword #:player-bullets #:init-thunk make-player-bullets)
  )

(define (make-shooter-scene)
  (make <shooter-scene>))

(define-method (draw (scene <shooter-scene>))
  (draw-player (player scene)))

(define-method (update (scene <shooter-scene>))
  (update-player! (player scene)))

(define-method (on-start (scene <shooter-scene>))
  (set! bitmap (al-load-bitmap "assets/sprite_sheets/player.png"))
  (set-player-sprite! (player scene) (make-sprite bitmap)))

(define-method (on-stop (scene <shooter-scene>))
  (al-destroy-bitmap bitmap))

(define-method (on-pause (scene <shooter-scene>))
  #f)

(define-method (on-resume (scene <shooter-scene>))
  #f)

(define-method (on-key-pressed (scene <shooter-scene>) keycode)
  (let ((player (player scene)))
    (cond ((= keycode allegro-key-up)
           (set-player-direction! player 'up #t))
          ((= keycode allegro-key-down)
           (set-player-direction! player 'down #t))
          ((= keycode allegro-key-left)
           (set-player-direction! player 'left #t))
          ((= keycode allegro-key-right)
           (set-player-direction! player 'right #t)))))

(define-method (on-key-released (scene <shooter-scene>) keycode)
  (let ((player (player scene)))
    (cond ((= keycode allegro-key-up)
           (set-player-direction! player 'up #f))
          ((= keycode allegro-key-down)
           (set-player-direction! player 'down #f))
          ((= keycode allegro-key-left)
           (set-player-direction! player 'left #f))
          ((= keycode allegro-key-right)
           (set-player-direction! player 'right #f))
          ((= keycode allegro-key-escape)
           (director-pop-scene)))))
