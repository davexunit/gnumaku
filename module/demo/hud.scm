(define-module (demo hud)
  #:use-module (srfi srfi-9)
  #:use-module (gnumaku core)
  #:use-module (gnumaku assets)
  #:use-module (gnumaku events)
  #:use-module (demo player)
  #:export (make-hud hud-width hud-height hud-player hud-font hud-life-image
                     set-hud-width! set-hud-height! set-hud-font! set-hud-life-image! draw-hud))

(define-record-type Hud
  (%make-hud player width height font life-image)
  hud?
  (player hud-player)
  (width hud-width set-hud-width!)
  (height hud-height set-hud-height!)
  (font hud-font set-hud-font!)
  (life-image hud-life-image set-hud-life-image!))

(define (make-hud player width height)
  (%make-hud player width height
             (load-asset "CarroisGothic-Regular.ttf" 14)
             (load-asset "heart.png")))

(define (draw-lives hud x y)
  (font-draw-text (hud-font hud)
                  x y '(1 1 1 1) "Lives")
  (let ((image (hud-life-image hud)))
    (let draw-life-icon ((i 0)
                         (x x)
                         (y (+ y 16)))
      (when (< i (lives (hud-player hud)))
        (draw-image image x y)
        (draw-life-icon (1+ i) (+ x (image-width image)) y)))))

(define (draw-graze hud x y)
    (font-draw-text (hud-font hud)
                  x y '(1 1 1 1) "Graze")
    (font-draw-text (hud-font hud)
                  x (+ y 16) '(1 1 1 1) (number->string (graze-count (hud-player hud)))))

(define (draw-hud hud)
  (draw-lives hud 520 20)
  (draw-graze hud 520 60))
