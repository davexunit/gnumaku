(define agenda (make-agenda))

(define (wait delay)
  (abort-to-prompt 'coroutine-prompt (lambda (resume)
				       (add-to-agenda! agenda delay resume))))
