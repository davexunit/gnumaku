(define-module (gnumaku bullet)
  #:use-module (gnumaku core)
  #:use-module (gnumaku math)
  #:export (get-bullet-type register-bullet-type emit-bullet emit-simple-bullet
                            emit-script-bullet bullet-wait))

(define bullet-types (make-hash-table))

(define (register-bullet-type key type)
  "Associates a key with a procedure for setting particular bullet properties."
  (hash-set! bullet-types key type))

(define (get-bullet-type key)
  "Applies the bullet properties associated with the given key."
  (hash-ref bullet-types key))

(define (bullet-wait bullet delay)
  "Sets the bullet script field to the continuation of a coroutine."
  (abort-to-prompt 'coroutine-prompt
                   (lambda (resume) (set-bullet-script bullet delay resume))))

(define (emit-bullet system pos speed direction acceleration angular-velocity life type)
  "Emits a non-scripted bullet."
  (let ((type (get-bullet-type type)))
    (%emit-bullet system pos speed direction acceleration angular-velocity life type)))

(define (emit-simple-bullet system pos speed direction type)
  "Emits a simple, non-scripted bullet with unlimited lifetime and no acceleration or angular velocity,"
  (let ((type (get-bullet-type type)))
    (%emit-simple-bullet system pos speed direction type)))

(define (emit-script-bullet system pos type script)
  "Emits a bullet that can be manipulated via a script."
  (let ((type (get-bullet-type type)))
    (%emit-script-bullet system pos type script)))
