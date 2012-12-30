(define-module (gnumaku scene-graph)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:export (<scene-node> <scene-group> position scale rotation parent %draw add-child remove-child))

(define-class <scene-node> ()
  (position #:accessor position #:init-keyword #:position #:init-value (make-vector2 0 0))
  (scale #:accessor scale #:init-keyword #:scale #:init-value (make-vector2 0 0))
  (rotation #:accessor rotation #:init-keyword #:rotation #:init-value 0)
  (parent #:accessor parent #:init-keyword #:parent #:init-value #f))

(define-method (update (node <scene-node>) dt))

(define-method (draw (node <scene-node>))
  (let ((transform (current-transform)))
    (apply-transform node)
    (%draw node)
    (use-transform transform)))

(define-method (%draw (node <scene-node>)))

(define-method (apply-transform (node <scene-node>))
  (let ((transform (current-transform))
        (pos (position node)))
    (translate-transform! transform (vector2-x pos) (vector2-y pos))
    (use-transform transform)))

(define-class <scene-group> (<scene-node>)
  (children #:accessor children #:init-keyword #:children #:init-value '()))

(define-method (add-child (group <scene-group>) node)
  (set! (parent node) group)
  (append! (children group) (list node)))

(define-method (remove-child (group <scene-group>) child-node)
  (delete! child-node (children group)))

(define-method (%draw (group <scene-group>))
  (for-each (lambda (node) (draw node)) (children group)))
