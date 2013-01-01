(define-module (demo hud)
  #:use-module (srfi srfi-9)
  #:use-module (gnumaku core)
  #:use-module (gnumaku assets)
  #:use-module (gnumaku events)
  #:use-module (demo level)
  #:use-module (demo player)
  #:export (make-hud hud-width hud-height hud-player hud-font hud-life-image
                     set-hud-width! set-hud-height! set-hud-font!
                     set-hud-life-image! draw-hud))

(define-record-type Hud
  (%make-hud level width height font life-image)
  hud?
  (level hud-level)
  (width hud-width set-hud-width!)
  (height hud-height set-hud-height!)
  (font hud-font set-hud-font!)
  (life-image hud-life-image set-hud-life-image!))

(define text-color (make-color-f 1 1 1 1))

(define (make-hud level width height)
  (%make-hud level width height
             (load-asset "CarroisGothic-Regular.ttf" 14)
             (load-asset "heart.png")))

(define (draw-lives hud x y)
  (font-draw-text (hud-font hud) x y text-color "Lives")
  (let ((image (hud-life-image hud))
        (player (player (hud-level hud))))
    (let draw-life-icon ((i 0)
                         (x x)
                         (y (+ y 16)))
      (when (< i (lives player))
        (draw-image image x y)
        (draw-life-icon (1+ i) (+ x (image-width image)) y)))))

(define (draw-graze hud x y)
  (let ((player (player (hud-level hud))))
    (font-draw-text (hud-font hud) x y text-color "Graze")
    (font-draw-text (hud-font hud) x (+ y 16) text-color
                  (number->string (graze-count player)))))

(define (draw-bullet-count hud x y)
  (define (bullet-count)
    (let ((level (hud-level hud)))
      (+ (bullet-system-count (player-bullet-system level))
         (bullet-system-count (enemy-bullet-system level)))))
  (font-draw-text (hud-font hud) x y text-color "Bullet Count")
  (font-draw-text (hud-font hud) x (+ y 16) text-color (number->string (bullet-count))))

(define (draw-hud hud)
  (draw-lives hud 520 20)
  (draw-graze hud 520 60)
  (draw-bullet-count hud 520 100))
