(define-module (gnumaku main)
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
  #:export (gmk-main
            *gmk-running*
            *gmk-display*))

(define *gmk-running* #t)
(define *gmk-display* #f)
(define window-title "Gnumaku!")

(define (gmk-main)
  ;; Create REPL server for live-coding awesomeness
  (spawn-server)

  ;; Initalize Allegro
  (al-init)
  (al-init-image-addon)
  (al-init-font-addon)
  (al-init-ttf-addon)
  (al-install-keyboard)
  (al-install-mouse)

  (let ((timer (al-create-timer (/ 1 60)))
        (events (al-create-event-queue))
        (redraw #t)
        (fps-time 0)
        (fps 0))
    ;; Set up
    (set! *gmk-running* #t)
    (set! *gmk-display* (al-create-display 640 480))
    (al-set-window-title *gmk-display* window-title)
    (al-register-event-source events (al-get-timer-event-source timer))
    (al-register-event-source events (al-get-keyboard-event-source))
    (al-start-timer timer)
    (set! fps-time (al-get-time))

    ;; Game loop
    (while *gmk-running*
      (let ((event (al-wait-for-event events)))
        (cond ((= (al-get-event-type event) allegro-event-key-up)
               ;(key-up (al-get-key-event-keycode (al-get-key-event event)))
               #f)
              ((= (al-get-event-type event) allegro-event-timer)
               (set! redraw #t)))
        (when (and redraw (al-is-event-queue-empty? events))
          (set! fps (1+ fps))
          (let ((current-time (al-get-time)))
            (when (>= (- current-time fps-time) 1)
              (al-set-window-title *gmk-display*
                                   (format #f "~A - ~d FPS" window-title fps))
              (set! fps-time current-time)
              (set! fps 0)))
          (set! redraw #f)
          (al-clear-to-color .1 .3 .5)
          (al-flip-display))))

    (al-destroy-event-queue events)
    (al-destroy-timer timer)
    (al-destroy-display *gmk-display*)))
