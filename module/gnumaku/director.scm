(define-module (gnumaku director)
  #:use-module (oop goops)
  #:use-module (gnumaku core)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku scene)
  #:use-module (gnumaku fps)
  #:use-module (gnumaku assets)
  #:export (director-init
            director-run
            director-pause
            director-resume
            director-show-fps
            director-push-scene
            director-pop-scene
            director-replace-scene
            director-set-draw-target
            director-reset-draw-target
            director-current-scene))

(define director-game (make-game))
(define director-scenes '())
(define director-fps (make-fps director-game))
(define director-font #f)
(define director-show-fps #f)

(define (director-current-scene)
  (if (null? director-scenes)
      #f
      (car director-scenes)))

(define (director-push-scene scene)
  ;; Stop current scene
  (when (director-current-scene)
    (on-stop (director-current-scene)))
  ;; Add new scene
  (set! director-scenes (cons scene director-scenes))
  ;; Start new scene
  (on-start scene))

(define (director-replace-scene scene)
  ;; Stop current scene
  (when (director-current-scene)
    (on-stop (director-current-scene)))
  ;; Replace current scene with new one
  (set! director-scenes (cons scene (cdr director-scenes)))
  ;; Start new scene
  (on-start scene))

(define (director-pop-scene)
  ;; Stop current scene
  (when (director-current-scene)
    (on-stop (director-current-scene)))
  ;; Remove scene
  (set! director-scenes (cdr director-scenes))
  (when (director-current-scene)
    (on-start (director-current-scene))))

(define* (director-init title width height #:optional (fullscreen #f))
  (game-init director-game title width height fullscreen))

(define (director-run scene)
  (director-push-scene scene)
  (game-run director-game))

(define (director-pause)
  (game-pause director-game))

(define (director-resume)
  (game-resume director-game))

(define (director-draw-fps)
  (font-draw-text director-font 730 575 (make-color-f 1 1 1 0.7)
                  (string-append "FPS: " (number->string (fps-last-frames director-fps)))))

(define (director-set-draw-target image)
  (set-target-image image))

(define (director-reset-draw-target)
  (game-reset-draw-target director-game))

(game-on-start-hook
 director-game
 (lambda ()
   (set! director-font (load-asset "CarroisGothic-Regular.ttf" 18))))

(game-on-update-hook
 director-game
 (lambda ()
   (update-fps! director-fps)
   ;; Update current scene. If the scene stack is empty, exit the game.
   (if (director-current-scene)
       (update (director-current-scene))
       (game-stop director-game))))

(game-on-draw-hook
 director-game
 (lambda ()
   (when (director-current-scene)
     (draw (director-current-scene)))
   (when director-show-fps
     (director-draw-fps))))

(game-on-key-pressed-hook
 director-game
 (lambda (key)
   (when (director-current-scene)
     (on-key-pressed (director-current-scene) key))))

(game-on-key-released-hook
 director-game
 (lambda (key)
   (when (director-current-scene)
     (on-key-released (director-current-scene) key))))

