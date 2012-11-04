(define (set-bullet-type! bullet key)
  "Provides a hash table of procedures that set bullet sprites and hitboxes"
  (define bullet-types (make-hash-table))

  (define (init-bullet-types)
    (hash-set! bullet-types 'medium-blue 
	       (lambda (bullet)
		 (set-bullet-sprite! bullet 0)
		 (set-bullet-hitbox! bullet -3 -3 6 6)))

    (hash-set! bullet-types 'small-diamond
	       (lambda (bullet)
		 (set-bullet-sprite! bullet 1)
		 (set-bullet-hitbox! bullet -2 -2 4 4)))

    (hash-set! bullet-types 'large-orange
	       (lambda (bullet)
		 (set-bullet-sprite! bullet 2)
		 (set-bullet-hitbox! bullet -5 -5 10 10))))

  ;; initalize bullet types the first time this procedure is called
  (unless (hash-ref bullet-types 'medium-blue)
    (init-bullet-types))

  ((hash-ref bullet-types key) bullet))
