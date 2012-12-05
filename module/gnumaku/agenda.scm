(define-module (gnumaku agenda)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-agenda agenda-schedule update-agenda clear-agenda))

;; This code is a modified version of the agenda implementation in SICP.
;; Thank you, SICP!

(define-record-type <time-segment>
  (%make-time-segment time queue)
  time-segment?
  (time segment-time)
  (queue segment-queue))

(define (make-time-segment time . callbacks)
  "Constructs a new time segment with given time and enqueus all callback procedures."
  (let ((segment (%make-time-segment time (make-q))))
    ;; Enqueue all callbacks
    (for-each (lambda (c) (segment-enq segment c)) callbacks)
    segment))

(define (segment-enq segment callback)
  "Enqueues a callback procedure onto the segment queue."
  (enq! (segment-queue segment) callback))

(define-record-type <agenda>
  (%make-agenda time segments)
  agenda?
  (time agenda-time set-agenda-time)
  (segments agenda-segments set-agenda-segments))

(define (make-agenda)
  "Creates a new empty agenda."
  (%make-agenda 0 '()))

(define (agenda-empty? agenda)
  "Returns #t if agenda has no scheduled procedures."
  (null? (agenda-segments agenda)))

(define (first-segment agenda)
  "Returns the first time segment in the agenda."
  (car (agenda-segments agenda)))

(define (rest-segments agenda)
  "Returns everything but the first segment in the agenda."
  (cdr (agenda-segments agenda)))

(define (agenda-add-segment agenda time callback)
  "Adds a new time segment to the beginning of the agenda and enqueues the given callback."
  (set-agenda-segments agenda (cons (make-time-segment time callback)
                                    (agenda-segments agenda))))

(define (insert-segment segments time callback)
  "Inserts a new segment after the first segment in the list."
  (set-cdr! segments (cons (make-time-segment time callback) (cdr segments))))

(define (first-agenda-item agenda)
  "Returns the first time segment queue in the agenda."
  (if (agenda-empty? agenda)
      (error "Agenda is empty")
      (segment-queue (first-segment agenda))))

(define (agenda-time-delay agenda dt)
  "Returns time given a delta from the current agenda time."
  (+ (agenda-time agenda) dt))

(define (agenda-schedule agenda dt callback)
  "Schedules a callback procedure in the agenda relative to the current agenda time."
  (let ((time (agenda-time-delay agenda dt)))
    (define (belongs-before? segments)
      "Determines if the time segment belongs before the first segment in the list"
      (or (null? segments)
          (< time (segment-time (car segments)))))

    (define (add-to-segments segments)
      "Schedules callback in the proper place."
      ;; Add to existing time segment if the times match
      (if (= (segment-time (car segments)) time)
          (segment-enq (car segments) callback)
          ;; Continue searching
          (if (belongs-before? (cdr segments))
              ;; Create new time segment and insert it where it belongs
              (insert-segment segments time callback)
              ;; Continue searching
              (add-to-segments (cdr segments)))))
    
    ;; Handle the case of inserting a new time segment at the beginning of the segment list
    (if (belongs-before? (agenda-segments agenda))
        ;; Add segment if it belongs at the beginning of the list...
        (agenda-add-segment agenda time callback)
        ;; ... Otherwise, search for the right place
        (add-to-segments (agenda-segments agenda)))))

(define (process-queue q)
  "Dequeues and executes every element of q."
  (unless (q-empty? q)
    ((deq! q)) ;; Execute scheduled procedure
    (process-queue q)))

(define (update-agenda agenda dt)
  "Moves agenda forward in time and run scheduled procedures."
  (set-agenda-time agenda (+ (agenda-time agenda) dt))
  (let next-segment ()
    (unless (agenda-empty? agenda)
      (let ((segment (first-segment agenda)))
        ;; Process time segment if it is scheduled before or at the current agenda time
        (when (>= (agenda-time agenda) (segment-time segment))
          (process-queue (segment-queue segment))
          (set-agenda-segments agenda (rest-segments agenda))
          (next-segment))))))

(define (clear-agenda! agenda)
  "Removes all scheduled procedures from the agenda."
  (set-agenda-segments agenda '()))
