(define-module (demo main))
(export main)

(load-extension "./gnumaku.so" "init_gnumaku_module")
(use-modules (system repl server) (gnumaku core) (gnumaku director) (gnumaku scene) (demo scenes shmup))

;; Seed random number generator
(set! *random-state* (random-state-from-platform))

;; Make a server for remote REPL
(spawn-server)

(define (main)
  ;; Start up director
  (director-init 800 600)
  (set! director-show-fps #t)
  (director-run (make-shmup-scene)))
