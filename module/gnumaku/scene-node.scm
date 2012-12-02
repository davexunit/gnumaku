(define-module (gnumaku scene-node)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:export (<scene-node> <scene-group> x y scale-x scale-y rotation parent %draw))

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

;; (define (layer-add-child layer child-layer)
;;   (set-layer-parent! child-layer layer)
;;   (let ((children (layer-children layer)))
;;     (append! children (cons child-layer '()))))

;; (define (layer-transform layer)
;;   (let ((transform (current-transform)))
;;     (translate-transform! transform (layer-x layer) (layer-y layer))
;;     (use-transform transform)))

;; (define (draw-layer layer)

;; (define* (draw-layers layers #:optional (x 0) (y 0))
;;   (unless (null? layers)
;;     (let ((layer (car layers)))
;;       (draw-layer layer)
;;       (draw-layers (cdr layers) (+ x (layer-x layer)) (+ y (layer-y layer))))))
