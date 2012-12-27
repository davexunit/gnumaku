(define-module (gnumaku primitives)
  #:use-module (gnumaku core)
  #:use-module (gnumaku math)
  #:use-module (gnumaku bullet-types)
  #:export (emit-bullet emit-simple-bullet emit-script-bullet bullet-wait))

(define (bullet-wait bullet delay)
  "Sets the bullet script field to the continuation of a coroutine."
  (abort-to-prompt 'coroutine-prompt
                   (lambda (resume) (set-bullet-script bullet delay resume))))

(define (emit-bullet system x y speed direction acceleration angular-velocity life type)
  "Emits a non-scripted bullet."
  (let ((type (get-bullet-type type)))
    (%emit-bullet system x y speed direction acceleration angular-velocity life type)))

(define (emit-simple-bullet system x y speed direction type)
  "Emits a simple, non-scripted bullet with unlimited lifetime and no acceleration or angular velocity,"
  (let ((type (get-bullet-type type)))
    (%emit-simple-bullet system x y speed direction type)))

(define (emit-script-bullet system x y type script)
  "Emits a bullet that can be manipulated via a script."
  (let ((type (get-bullet-type type)))
    (%emit-script-bullet system x y type script)))
