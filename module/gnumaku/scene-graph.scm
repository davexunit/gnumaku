(define-module (gnumaku scene-graph)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:export (<scene-node> <scene-group> x y scale-x scale-y rotation parent %draw add-child remove-child))

(define-class <scene-node> ()
  (x #:accessor x #:init-keyword #:x #:init-value 0)
  (y #:accessor y #:init-keyword #:y #:init-value 0)
  (scale-x #:accessor scale-x #:init-keyword #:scale-x #:init-value 1)
  (scale-y #:accessor scale-y #:init-keyword #:scale-y #:init-value 1)
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
  (let ((transform (current-transform)))
    (translate-transform! transform (x node) (y node))
    (use-transform transform)))

(define-method (set-position (node <scene-node>) new-x new-y)
  (set! (x node) new-x)
  (set! (y node) new-y))

(define-class <scene-group> (<scene-node>)
  (children #:accessor children #:init-keyword #:children #:init-value '()))

(define-method (add-child (group <scene-group>) node)
  (set! (parent node) group)
  (append! (children group) (list node)))

(define-method (remove-child (group <scene-group>) child-node)
  (delete! child-node (children group)))

(define-method (%draw (group <scene-group>))
  (for-each (lambda (node) (draw node)) (children group)))
