(define-module (demo enemies)
  #:export (make-enemy-1))

(use-modules (gnumaku core) (gnumaku enemy) (gnumaku primitives) (gnumaku coroutine))

(define (make-enemy-1 x y image)
  (define (action enemy)
    (coroutine
     (let loop ()
       (let ((x (enemy-x enemy))
             (y (enemy-y enemy))
             (system (enemy-bullet-system enemy)))
         (emit-circle system x y 32 64 (random 20) 50 0 (- (random 20) 10) 'small-diamond)
         (enemy-wait enemy 60)
         (loop)))))

  (let ((enemy (make-enemy image action 50 100)))
    (set-enemy-position! enemy x y)
    enemy))
