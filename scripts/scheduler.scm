(use-modules (ice-9 receive) (ice-9 q) (srfi srfi-1))

;; Thank you, SICP!

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time segment)
  (car segment))

(define (segment-queue segment)
  (cdr segment))

(define (make-agenda)
  (list 0))

(define (agenda-empty? agenda)
  (null? (segments agenda)))

(define (current-time agenda)
  (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda)
  (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda)
  (car (segments agenda)))

(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty")
      (let ((first-seg (first-segment agenda)))
	(segment-queue first-seg))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    ((deq! q))
    (if (q-empty? q)
	(set-segments! agenda (rest-segments agenda)))))

(define (add-to-agenda! agenda time callback)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))

  (define (make-new-time-segment time callback)
    (let ((q (make-q)))
      (enq! q callback)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
	(enq! (segment-queue (car segments)) callback)
	(let ((rest (cdr segments)))
	  (if (belongs-before? rest)
	      (set-cdr! segments 
			(cons (make-new-time-segment time callback)
			      (cdr segments)))
	      (add-to-segments! rest)))))
  
  (set! time (+ time (current-time agenda)))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
	(set-segments! agenda
		       (cons (make-new-time-segment time callback)
			     segments))
	(add-to-segments! segments))))

(define (update-agenda agenda dt)
  (define (process-queue q)
    (unless (q-empty? q)
      ((deq! q))
      (process-queue q)))

  (set-current-time! agenda (+ (current-time agenda) dt))
  (let loop ()
    (if (agenda-empty? agenda)
	"Agenda is empty"
	(let ((segment (first-segment agenda)))
	  (when (>= (current-time agenda) (segment-time segment))
	    (process-queue (segment-queue segment))
	    (set-segments! agenda (rest-segments agenda))
	    (loop))))))

