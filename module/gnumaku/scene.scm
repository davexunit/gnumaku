(define-module (gnumaku scene)
  #:use-module (oop goops)
  #:use-module (gnumaku generics)
  #:export (<scene> name on-start on-stop on-pause on-resume on-key-pressed on-key-released))

(define-class <scene> ()
  (name #:accessor name #:init-keyword #:name #:init-value "untitled"))

(define-method (draw (scene <scene>)))

(define-method (update (scene <scene>)))

(define-method (on-start (scene <scene>)))

(define-method (on-stop (scene <scene>)))

(define-method (on-pause (scene <scene>)))

(define-method (on-resume (scene <scene>)))

(define-method (on-key-pressed (scene <scene>) key))

(define-method (on-key-released (scene <scene>) key))
