(define-module (gnumaku director)
  #:use-module (system repl server)
  #:use-module (allegro system)
  #:use-module (allegro display)
  #:use-module (allegro events)
  #:use-module (allegro keyboard)
  #:use-module (allegro mouse)
  #:use-module (allegro timer)
  #:use-module (allegro time)
  #:use-module (allegro graphics)
  #:use-module (allegro addons image)
  #:use-module (allegro addons font)
  #:use-module (allegro addons ttf)
  #:use-module (oop goops)
  #:use-module (gnumaku scene)
  #:export (director-init
            director-run
            director-pause
            director-resume
            director-stop
            director-show-fps
            director-push-scene
            director-pop-scene
            director-replace-scene
            director-set-draw-target
            director-reset-render-image
            director-current-scene))

(define *director-running* #t)
(define *director-paused* #f)
(define *director-display* #f)
(define *director-window-title* #f)
(define *director-timer* #f)
(define *director-timestep* (/ 1 60))
(define director-scenes '())

(define (game-loop)
  (let ((events (al-create-event-queue))
        (redraw #t)
        (fps-time 0)
        (fps 0))
    ;; Set up
    (set! *director-running* #t)
    (set! *director-timer* (al-create-timer *director-timestep*))
    (al-register-event-source events (al-get-timer-event-source *director-timer*))
    (al-register-event-source events (al-get-keyboard-event-source))
    (al-start-timer *director-timer*)
    (set! fps-time (al-get-time))
    ;; Game loop
    (while *director-running*
      ;; Event handling
      (let* ((event (al-wait-for-event events))
             (event-type (al-get-event-type event)))
        (unless *director-paused*
          (cond ((= event-type allegro-event-key-down)
                 (director-key-pressed
                  (al-get-key-event-keycode (al-get-key-event event))))
                ((= event-type allegro-event-key-up)
                 (director-key-released
                  (al-get-key-event-keycode (al-get-key-event event))))
                ((= event-type allegro-event-timer)
                 (director-update)
                 (set! redraw #t)))))
      ;; Rendering
      (when (and redraw (al-is-event-queue-empty? events))
        ;; Update FPS
        (set! fps (1+ fps))
        (let ((current-time (al-get-time)))
          (when (>= (- current-time fps-time) 1)
            (al-set-window-title *director-display*
                                 (format #f "~A - ~d FPS"
                                         *director-window-title* fps))
            (set! fps-time current-time)
            (set! fps 0)))
        ;; Draw
        (set! redraw #f)
        (director-draw)))
    ;; Clean up
    (al-destroy-event-queue events)
    (al-destroy-timer *director-timer*)))

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

(define* (director-init #:optional #:key (title "Gnumaku!")
                        (width 640) (height 480) (fullscreen #f))
  ;; Initalize Allegro
  (al-init)
  (al-init-image-addon)
  (al-init-font-addon)
  (al-init-ttf-addon)
  (al-install-keyboard)
  (al-install-mouse)

  ;; Create display
  (set! *director-display* (al-create-display width height))
  (set! *director-window-title* title)
  (al-set-window-title *director-display* title))

(define (director-terminate)
  (al-destroy-display *director-display*))

(define (director-run scene)
  (director-push-scene scene)
  (game-loop)
  (director-terminate))

(define (director-pause)
  (al-stop-timer *director-timer*)
  (set! *director-paused* #t))

(define (director-resume)
  (al-start-timer *director-timer*)
  (set! *director-paused* #f))

(define (director-stop)
  (set! *director-running* #f))

(define (director-update)
  ;; Update current scene. If the scene stack is
  ;; empty, exit the game.
  (if (director-current-scene)
      (update (director-current-scene))
      (director-stop)))

(define (director-draw)
  (when (director-current-scene)
    (al-clear-to-color 0 0 0)
    (draw (director-current-scene))
    (al-flip-display)))

(define (director-key-pressed keycode)
  (when (director-current-scene)
    (on-key-pressed (director-current-scene) keycode)))

(define (director-key-released keycode)
  (when (director-current-scene)
    (on-key-released (director-current-scene) keycode)))
