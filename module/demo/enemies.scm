(define-module (demo enemies)
  #:export (make-enemy-1))

(use-modules (gnumaku core) (gnumaku enemy) (gnumaku primitives) (gnumaku coroutine))

(define (make-enemy-1 x y image)
  (define (action enemy)
    (coroutine
     (let loop ((rotate 0))
       (let ((x (enemy-x enemy))
             (y (enemy-y enemy))
             (system (enemy-bullet-system enemy)))
         (emit-circle system x y 32 16 rotate 50 5 5 'medium-blue)
         ;; (emit-circle system x y 32 16 (+ rotate 4) 50 5 5 'small-diamond)
         (enemy-wait enemy 15)
         (loop (+ rotate 10))))))

  (let ((enemy (make-enemy image action 50 100)))
    (set-enemy-position! enemy x y)
    enemy))
