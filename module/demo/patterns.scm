(define-module (demo patterns)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:use-module (gnumaku core)
  #:use-module (gnumaku math)
  #:use-module (gnumaku bullet)
  #:use-module (gnumaku coroutine)
  #:use-module (gnumaku scene-graph)
  #:use-module (demo actor)
  #:use-module (demo level)
  #:export (spiral1 spiral2 arch-spiral double-spiral fire-at-player))

(define (clamp n min-n max-n)
  (max min-n (min max-n n)))

(define-coroutine (homing-bullet bullet target speed turn)
  (let* ((pos (bullet-position bullet))
         (target-pos (vector2-sub (position target) pos))
         (turn (if (> (vector2-cross pos target-pos) 0) turn (* -1 turn))))
    (set-bullet-movement bullet speed (+ (bullet-direction bullet) turn) 0 0))
  (bullet-wait bullet 1)
  (homing-bullet bullet target speed turn))

(define-coroutine (sine-wave bullet)
  (define angle-step 20)
  
  (define (step angle)
    (set-bullet-direction bullet (+ (bullet-direction bullet) (* 8 (sin-deg angle))))
    (bullet-wait bullet 1)
    (step (+ angle angle-step)))

  (step 0))

(define-coroutine (explode bullet system delay count speed angle-var ang-vel)
  (define step (/ 360 count))
  
  (define (emit i)
    (let ((direction (+ (* i step) (random angle-var))))
      (emit-bullet system (bullet-position bullet) speed direction 'sword
                   #:angular-velocity ang-vel)))
  
  (bullet-wait bullet delay)
  (kill-bullet bullet)
  (repeat count emit))

(define-coroutine (spiral1 actor)
  (let ((step (/ 360 15)))
    (define (spiral angle)
      (let ((system (bullet-system actor))
            (player (player (level actor)))
            (direction (+ 90 (* 30 (sin-deg angle)))))
        (emit-script-bullet system (position actor) 'bright
                            (coroutine (bullet)
                              (set-bullet-movement bullet 2 direction 0 0)
                              (set-bullet-color bullet
                                                (make-color-f (random:exp) (random:exp)
                                                              (random:exp) 1))
                              ;;(homing-bullet bullet player 3 1))))
                              (bullet-wait bullet 30)
                              (set-bullet-type bullet 'small-green)
                              (set-bullet-color bullet (make-color-f 1 1 1 1))
                              (explode bullet system 45 6 2.5 30 .3))))
                              ;; (sine-wave bullet))))
      (wait actor 6)
      (spiral (+ angle step)))
    (spiral 0)))

(define-coroutine (spiral2 actor)
  (let loop ((angle 0))
    (let ((scale (* 2 (random:exp))))
      (emit-bullet (bullet-system actor) (position actor) 2
                   (random (+ angle 20)) 'medium-blue
                   #:color (make-color-f (random:exp) (random:exp) (random:exp) 1)
                   #:scale (make-vector2 scale scale)))
    (wait actor 2)
    (loop (+ angle 20))))

(define-coroutine (arch-spiral actor)
  (define step (quotient 360 20))
  
  (define-coroutine (script bullet angle)
    (set-bullet-movement bullet 0 angle 0 0)
    (bullet-wait bullet 15)
    (set-bullet-movement bullet 3 angle 0 0))

  (define (emit angle)
    (let* ((angle (+ angle (random 20)))
           (a .01)
           (b .1)
           (system (bullet-system actor))
           (r (+ a (* b angle)))
           (pos (vector2-add (position actor)
                             (vector2-from-polar r angle))))
      (emit-script-bullet system pos 'medium-blue
                          (lambda (bullet) (script bullet angle)))))
  
  (define (spiral angle)
      (when (< angle (* 3 360))
        (emit angle)
        (wait actor 1)
        (spiral (+ angle step))))
  
  (spiral 0)
  (arch-spiral actor))

(define-coroutine (double-spiral actor)
  (define (emit i total offset color)
    (let* ((angle (+ offset (* 360 (/ i total))))
           (pos (vector2-add (position actor) (vector2-from-polar 50 angle))))
      (emit-bullet (bullet-system actor) pos 3 angle 'medium-blue #:color color)))

  (define (spiral angle-a angle-b)
    (let ((step (/ 360 20))
          (num-bullets 16)
          (color-a (make-color-f 1 0 0 1))
          (color-b (make-color-f 1 1 1 1)))
      (repeat num-bullets (lambda (i) (emit i num-bullets angle-a color-a)))
      (repeat num-bullets (lambda (i) (emit i num-bullets angle-b color-b)))
      (wait actor 10)
      (spiral (+ angle-a step) (- angle-b step))))

  (spiral 0 0))
