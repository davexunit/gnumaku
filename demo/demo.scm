(define-module (demo)
  #:use-module (system repl server)
  #:use-module (gnumaku director)
  #:use-module (demo scenes shooter)
  #:export (run-demo))

(define (run-demo)
  ;; Create REPL server for live-coding awesomeness.
  (spawn-server)
  ;; Start game.
  (director-init)
  (director-run (make-shooter-scene)))
