(define-module (demo hud)
  #:export (make-hud hud-width hud-height hud-player hud-font hud-life-image
                     set-hud-width! set-hud-height! set-hud-font! set-hud-life-image! draw-hud))

(use-modules (srfi srfi-9) (gnumaku core) (gnumaku player))

(define life-icon #f)
(define font #f)

(define-record-type Hud
  (%make-hud width height player font life-image)
  hud?
  (width hud-width set-hud-width!)
  (height hud-height set-hud-height!)
  (player hud-player)
  (font hud-font set-hud-font!)
  (life-image hud-life-image set-hude-life-image!))

(define (make-hud width height player)
  (unless font
    (set! font (make-font "data/fonts/CarroisGothic-Regular.ttf" 14)))
  (unless life-icon
    (set! life-icon (load-image "data/images/heart.png")))
  (%make-hud width height player font life-icon))

(define (draw-lives x y hud)
  (font-draw-text (hud-font hud) x y '(1 1 1 1) "Lives")
  (let ((image (hud-life-image hud)))
    (let draw-life-icon ((i 0)
                         (x x)
                         (y (+ y 16)))
      (when (< i (lives (hud-player hud)))
        (draw-image image x y)
        (draw-life-icon (1+ i) (+ x (image-width image)) y)))))

(define (draw-hud hud)
  (draw-lives 520 20 hud))
