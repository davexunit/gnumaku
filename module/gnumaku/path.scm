(define-module (gnumaku path)
  #:use-module (srfi srfi-9)
  #:use-module (gnumaku vector)
  #:export (make-bezier bezier-at))

(define-record-type <bezier>
  (make-bezier p0 p1 p2 p3)
  bezier?
  (p0 bezier-p0)
  (p1 bezier-p1)
  (p2 bezier-p2)
  (p3 bezier-p3))

(define (bezier-at bezier t)
  "Returns the position along the bezier curve at time t, where t is 0 <= t <= 1."
  (let ((u (- 1 t)))
    (vadd (vscale (* u u u) (bezier-p0 bezier))
          (vscale (* 3 u u t) (bezier-p1 bezier))
          (vscale (* 3 u t t) (bezier-p2 bezier))
          (vscale (* t t t) (bezier-p3 bezier)))))
