(define-module (gnumaku vector)
  #:use-module (srfi srfi-1)
  #:export (vadd vmul vscale vmag vnorm))

(define (vadd . vectors)
  "Adds vectors"
  (apply map + vectors))

(define (vmul . vectors)
  "Multiplies vectors"
  (apply map * vectors))

(define (vscale scalar vector)
  "Multiplies a vector by a scalar"
  (map (lambda (e) (* scalar e)) vector))

(define (vmag vector)
  "Returns the magnitude of a vector"
  (sqrt (reduce + 0 (map (lambda (e) (* e e)) vector))))

(define (vnorm vector)
  (let ((m (vmag vector)))
    (map (lambda (e) (/ e m)) vector)))
