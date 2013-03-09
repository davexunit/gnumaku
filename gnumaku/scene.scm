(define-module (gnumaku scene)
  #:use-module (oop goops)
  #:export (draw
            update
            on-start
            on-stop
            on-pause
            on-resume
            on-key-pressed
            on-key-released))

(define-generic draw)
(define-generic update)
(define-generic on-start)
(define-generic on-stop)
(define-generic on-pause)
(define-generic on-resume)
(define-generic on-key-pressed)
(define-generic on-key-released)
