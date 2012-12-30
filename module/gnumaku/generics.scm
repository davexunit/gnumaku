;; This module is necessary because I can't resolve GOOPS generic method duplications in a satisfactory way.
(define-module (gnumaku generics)
  #:use-module (oop goops)
  #:export (update draw))

(define-generic draw)
(define-generic update)
