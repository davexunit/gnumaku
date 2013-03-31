(define-module (gnumaku)
  #:use-module (allegro graphics)
  #:export (emit-particle!))

(load-extension "./libgnumaku" "gmk_init")

;; Wrapper around %emit-particle that first unwraps the allegro
;; bitmap.
(define (emit-particle! particle-system pos speed direction accel
                        ang-vel bitmap)
  (%emit-particle! particle-system pos speed direction accel ang-vel
                   (unwrap-allegro-bitmap bitmap)))
