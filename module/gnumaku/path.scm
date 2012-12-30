(define-module (gnumaku path)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (gnumaku core)
  #:export (make-bezier make-bezier-path bezier-at bezier-path draw-bezier draw-bezier-path))

(define-record-type <bezier>
  (make-bezier p0 p1 p2 p3)
  bezier?
  (p0 bezier-p0)
  (p1 bezier-p1)
  (p2 bezier-p2)
  (p3 bezier-p3))

(define (bezier-at bezier t)
  "Returns the position along the bezier curve at time t, where 0 <= t <= 1."
  (let ((u (- 1 t)))
    (vector2-add (vector2-scale (bezier-p0 bezier) (* u u u) )
                 (vector2-scale (bezier-p1 bezier) (* 3 u u t))
                 (vector2-scale (bezier-p2 bezier) (* 3 u t t))
                 (vector2-scale (bezier-p3 bezier) (* t t t)))))

(define (make-bezier-path . points)
  "Builds a list of bezier curves from the given points that form a connected path."
  (define (build-path points path)
    (if (>= (length points) 4)
      (build-path (drop points 3)
                  (append path
                          (list (make-bezier (first points) (second points)
                                             (third points) (fourth points)))))
      path))
  (build-path points '()))

(define* (draw-bezier bezier #:optional #:key (segments 32) (color '(1 1 1 1)) (thickness 2))
  "Draws a bezier curve approximation as a series of line segments."
  (let draw-segment ((i 1)
                     (last (bezier-at bezier 0)))
    (when (<= i segments)
      (let ((current (bezier-at bezier (/ i segments))))
        (draw-line (vector2-x last) (vector2-y last)
                   (vector2-x current) (vector2-y current)
                   color thickness)
        (draw-segment (1+ i) current)))))

(define* (draw-bezier-path bezier-path #:optional #:key (segments 32)
                           (color '(1 1 1 1)) (thickness 2))
  "Draws a series of connected bezier curves."
  (for-each (lambda (b) (draw-bezier b #:segments segments
                                     #:color color #:thickness thickness))
            bezier-path))


