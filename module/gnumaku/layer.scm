(define-module (gnumaku layer)
  #:export (make-layer layer? layer-rect set-layer-rect! layer-draw-proc set-layer-draw-proc!
                       layer-children layer-clip? set-layer-clip! draw-layer draw-layers))

(use-modules (srfi srfi-9) (gnumaku core))

(define-record-type Layer
  (%make-layer rect draw-proc children parent clip)
  layer?
  (rect layer-rect set-layer-rect!)
  (draw-proc layer-draw-proc set-layer-draw-proc!)
  (children layer-children)
  (parent layer-parent set-layer-parent!)
  (clip layer-clip? set-layer-clip!))

(define (make-layer rect draw-proc)
  (%make-layer rect draw-proc '() #f #f))

(define (layer-x layer)
  (rect-x (layer-rect layer)))

(define (layer-y layer)
  (rect-y (layer-rect layer)))

(define (layer-add-child layer child-layer)
  (set-layer-parent! child-layer layer)
  (let ((children (layer-children layer)))
    (append! children (cons child-layer '()))))

(define (layer-transform layer)
  (let ((transform (current-transform)))
    (translate-transform! transform (layer-x layer) (layer-y layer))
    (use-transform transform)))

(define (draw-layer layer)
  (let ((transform (current-transform)))
    ;; Apply transformation
    (layer-transform layer)
    ;; Draw self
    (when (layer-clip? layer)
      (set-clipping-rect (layer-rect layer)))
    ((layer-draw-proc layer))
    (when (layer-clip? layer)
      (reset-clipping-rect))
    ;; Draw children
    (draw-layers (layer-children layer))
    ;; Reset transform
    (use-transform transform)))

(define* (draw-layers layers #:optional (x 0) (y 0))
  (unless (null? layers)
    (let ((layer (car layers)))
      (draw-layer layer)
      (draw-layers (cdr layers) (+ x (layer-x layer)) (+ y (layer-y layer))))))
