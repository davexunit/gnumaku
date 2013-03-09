(define-module (gnumaku sprite)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (allegro graphics)
  #:export (make-sprite
            sprite?
            sprite-bitmap
            set-sprite-bitmap!
            sprite-position
            set-sprite-position!
            sprite-scale
            set-sprite-scale!
            sprite-rotation
            set-sprite-rotation!
            draw-sprite))

(define-record-type <sprite>
  (%make-sprite bitmap position scale rotation)
  sprite?
  (bitmap sprite-bitmap set-sprite-bitmap!)
  (position sprite-position set-sprite-position!)
  (scale sprite-scale set-sprite-scale!)
  (rotation sprite-rotation set-sprite-rotation!))

(define* (make-sprite bitmap #:optional #:key (position '(0 0))
                      (scale '(1 1)) (rotation 0))
  (%make-sprite bitmap position scale rotation))

(define (draw-sprite sprite)
  (let* ((bitmap (sprite-bitmap sprite))
         (cx (/ (al-get-bitmap-width bitmap)  2))
         (cy (/ (al-get-bitmap-height bitmap) 2))
         (x (first (sprite-position sprite)))
         (y (second (sprite-position sprite)))
         (sx (first (sprite-scale sprite)))
         (sy (second (sprite-scale sprite)))
         (theta (sprite-rotation sprite)))
   (al-draw-scaled-rotated-bitmap bitmap cx cy x y sx sy theta 0)))
