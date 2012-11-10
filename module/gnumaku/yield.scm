(define-module (gnumaku yield))
(use-modules (gnumaku scheduler))
(export wait wait-scheduler)

(define wait-scheduler (make-agenda))

(define (wait delay)
  (abort-to-prompt 'coroutine-prompt (lambda (resume)
				       (add-to-agenda! wait-scheduler delay resume))))
