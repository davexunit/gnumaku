(define-module (gnumaku bullet)
  #:use-module (gnumaku core)
  #:use-module (gnumaku math)
  #:export (get-bullet-type
            register-bullet-type
            emit-bullet
            emit-script-bullet
            bullet-wait
            set-bullet-type))

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

(define* (emit-bullet system pos speed direction type . keyword-args)
  "Emits a non-scripted bullet."
  (let ((type (get-bullet-type type)))
    (apply %emit-bullet system pos speed direction type keyword-args)))

(define (emit-script-bullet system pos type script)
  "Emits a bullet that can be manipulated via a script."
  (let ((type (get-bullet-type type)))
    (%emit-script-bullet system pos type script)))

(define (set-bullet-type bullet type)
  (let ((type (get-bullet-type type)))
    (%set-bullet-type bullet type)))
