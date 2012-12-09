(define-module (gnumaku actions)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gnumaku core)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku scene-graph)
  #:use-module (gnumaku coroutine)
  #:use-module (gnumaku path)
  #:export (move-to move-by move-bezier move-bezier-path))

(define-method (move-to (node <scene-node>) dst-x dst-y duration)
  "Moves linearly from current position to destination."
  (let ((start-x (x node))
        (start-y (y node))
        (dx (- dst-x (x node)))
        (dy (- dst-y (y node))))
    (let tick ((t 0))
      (when (< t duration)
        (let ((alpha (/ t duration)))
          (set! (x node) (+ start-x (* dx alpha)))
          (set! (y node) (+ start-y (* dy alpha))))
        (wait node 1)
        (tick (1+ t))))))

(define-method (move-by (node <scene-node>) dx dy duration)
  "Moves linearly from current position by (dx, dy)."
  (move-to node (+ (x node) dx) (+ (y node) dy) duration))

(define-method (move-bezier (node <scene-node>) bezier duration)
  "Moves along a bezier curve."
  (let tick ((t 0))
    (when (< t duration)
      (let ((p (bezier-at bezier (/ t duration))))
        (set! (x node) (first p))
        (set! (y node) (second p)))
      (wait node 1)
      (tick (1+ t)))))

(define-method (move-bezier-path (node <scene-node>) beziers durations)
  "Moves along a series of connected bezier curves."
  (for-each (lambda (b d) (move-bezier node b d)) beziers durations))
