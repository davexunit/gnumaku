(define-module (gnumaku scene)
  #:export (make-scene scene-on-start scene-on-stop scene-on-pause scene-on-resume
                       scene-on-update scene-on-draw scene-on-key-pressed scene-on-key-released
                       scene-running? scene-paused? scene-start scene-stop scene-pause scene-resume
                       scene-update draw-scene scene-key-pressed scene-key-released
                       scene-on-start-hook scene-on-stop-hook scene-on-pause-hook scene-on-resume-hook
                       scene-on-update-hook scene-on-draw-hook scene-on-key-pressed-hook scene-on-key-released-hook))
(use-modules (srfi srfi-9) (gnumaku core))

(define-record-type Scene
  (%make-scene on-start on-stop on-pause on-resume on-update on-draw
               on-key-pressed on-key-released running paused)
  scene?
  (on-start scene-on-start scene-on-start-hook)
  (on-stop scene-on-stop scene-on-stop-hook)
  (on-pause scene-on-pause scene-on-pause-hook)
  (on-resume scene-on-resume scene-on-resume-hook)
  (on-update scene-on-update scene-on-update-hook)
  (on-draw scene-on-draw scene-on-draw-hook)
  (on-key-pressed scene-on-key-pressed scene-on-key-pressed-hook)
  (on-key-released scene-on-key-released scene-on-key-released-hook)
  (running scene-running? set-scene-running!)
  (paused scene-paused? set-scene-paused!))

(define (make-scene)
  (%make-scene #f #f #f #f #f #f #f #f #f #f))

(define (scene-update scene dt)
  (when (scene-on-update scene)
    ((scene-on-update scene) dt)))

(define (draw-scene scene)
  (when (scene-on-draw scene)
    ((scene-on-draw scene))))

(define (scene-start scene)
  (when (scene-on-start scene)
    (set-scene-running! scene #t)
    ((scene-on-start scene))))

(define (scene-stop scene)
  (when (scene-on-stop scene)
    (set-scene-running! scene #f)
    ((scene-on-stop scene))))

(define (scene-pause scene)
  (when (scene-on-pause scene)
    (set-scene-paused! scene #t)
    ((scene-on-pause scene))))

(define (scene-resume scene)
  (when (scene-on-resume scene)
    (set-scene-paused! scene #f)
    ((scene-on-resume scene))))

(define (scene-key-pressed scene key)
  (when (scene-on-key-pressed scene)
    ((scene-on-key-pressed scene) key)))

(define (scene-key-released scene key)
  (when (scene-on-key-released scene)
    ((scene-on-key-released scene) key)))
