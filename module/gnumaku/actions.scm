(define-module (gnumaku actions)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnumaku core)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku scene-graph)
  #:use-module (gnumaku coroutine)
  #:use-module (gnumaku path)
  #:export (move-to move-by move-bezier move-bezier-path sprite-blink))

(define-method (move-to (node <scene-node>) dest duration)
  "Moves linearly from current position to destination."
  (let* ((start (position node))
         (distance (vector2-sub dest start)))
    (let tick ((t 0))
      (when (< t duration)
        (let ((alpha (/ t duration)))
          (set! (position node) (vector2-add start (vector2-scale distance alpha))))
        (wait node 1)
        (tick (1+ t))))))

(define-method (move-by (node <scene-node>) distance duration)
  "Moves linearly from current position by (dx, dy)."
  (move-to node (vector2-add (position node) distance) duration))

(define-method (move-bezier (node <scene-node>) bezier duration)
  "Moves along a bezier curve."
  (let tick ((t 0))
    (when (< t duration)
      (let ((p (bezier-at bezier (/ t duration))))
        (set! (position node) p))
      (wait node 1)
      (tick (1+ t)))))

(define-method (move-bezier-path (node <scene-node>) beziers durations)
  "Moves along a series of connected bezier curves."
  (for-each (lambda (b d) (move-bezier node b d)) beziers durations))

(define-method (sprite-blink (node <scene-node>) sprite interval times)
  (define (blink i visible)
    (when (< i times)
      (set-sprite-opacity sprite (if visible 1 0))
      (wait node interval)
      (blink (1+ i) (not visible))))
  (blink 0 #f)
  (set-sprite-opacity sprite 1))
