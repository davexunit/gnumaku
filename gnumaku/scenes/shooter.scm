(define-module (gnumaku scenes shooter)
  #:use-module (oop goops)
  #:use-module (allegro keyboard)
  #:use-module (gnumaku scene)
  #:use-module (gnumaku director)
  #:export (make-shooter-scene))

(define-class <shooter-scene> ()
  (player #:accessor player #:init-keyword #:player #:init-value #f))

(define (make-shooter-scene)
  (make <shooter-scene>))

(define-method (draw (scene <shooter-scene>))
  #f)

(define-method (update (scene <shooter-scene>))
  #f)

(define-method (on-start (scene <shooter-scene>))
  #f)

(define-method (on-stop (scene <shooter-scene>))
  #f)

(define-method (on-pause (scene <shooter-scene>))
  #f)

(define-method (on-resume (scene <shooter-scene>))
  #f)

(define-method (on-key-pressed (scene <shooter-scene>) keycode)
  #f)

(define-method (on-key-released (scene <shooter-scene>) keycode)
  (when (= keycode allegro-key-escape)
    (director-pop-scene)))
