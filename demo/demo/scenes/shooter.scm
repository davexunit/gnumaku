(define-module (demo scenes shooter)
  #:use-module (srfi srfi-42)
  #:use-module (oop goops)
  #:use-module (allegro keyboard)
  #:use-module (allegro graphics)
  #:use-module (allegro addons image)
  #:use-module (gnumaku)
  #:use-module (gnumaku scene)
  #:use-module (gnumaku director)
  #:use-module (gnumaku sprite)
  #:use-module (gnumaku player)
  #:use-module (gnumaku agenda)
  #:use-module (gnumaku coroutine)
  #:export (make-shooter-scene
            player
            player-bullets))

(define bitmap #f)

(define (make-default-player)
  (make-player #f '(320 460) 4 1 3 #f #f))

(define (make-player-bullets)
  (make-particle-system 60000))

(define (make-bullet-image)
  (al-load-bitmap "assets/sprite_sheets/bullet.png"))

(define-class <shooter-scene> ()
  (player #:accessor player #:init-keyword #:player #:init-thunk make-default-player)
  (player-bullets #:accessor player-bullets #:init-keyword #:player-bullets #:init-thunk make-player-bullets)
  (bullet-image #:accessor bullet-image #:init-keyword #:bullet-image)
  (agenda #:accessor agenda #:init-thunk make-agenda))

(define (make-shooter-scene)
  (make <shooter-scene>))

(define-method (draw (scene <shooter-scene>))
  (al-clear-to-color .1 .4 .5)
  (draw-particle-system (player-bullets scene))
  (draw-player (player scene)))

(define-method (update (scene <shooter-scene>))
  (update-agenda! (agenda scene))
  (update-particle-system! (player-bullets scene))
  (update-player! (player scene)))

(define-method (on-start (scene <shooter-scene>))
  (set! bitmap (al-load-bitmap "assets/sprite_sheets/player.png"))
  (set! (bullet-image scene) (make-bullet-image))
  (set-player-sprite! (player scene) (make-sprite bitmap))
  (fire-circle scene))

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

(define-method (fire-circle (scene <shooter-scene>))
  (define num-bullets 32)

  (define (fire-bullet i)
    (emit-particle! (player-bullets scene) '(320 240) 2 (* 360 (/ i num-bullets))
                    (bullet-image scene) #:ang-vel .5)
    (wait))

  (define-coroutine (fire)
    (do-ec (: i num-bullets) (fire-bullet i))
    (fire))

  (agenda-schedule! (agenda scene) fire 0))

(define-method (fire-a-bunch (scene <shooter-scene>))
  (let ((n 40000))
    (let loop ((i 0))
      (when (< i n)
        (emit-particle! (player-bullets scene) (list (random 640) (random 480)) .4 (random 360) (bullet-image scene))
        (loop (1+ i))))))
