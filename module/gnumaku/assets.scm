(define-module (gnumaku assets)
  #:use-module (srfi srfi-9)
  #:use-module (gnumaku core)
  #:export (load-asset))

(define-record-type <asset-manager>
  (make-asset-manager file-types load-proc dict)
  asset-manager?
  (file-types asset-manager-file-types)
  (load-proc asset-manager-load-proc)
  (dict asset-manager-dict))

(define (asset-manager-match-type? manager filename)
  "Checks if the manager can load the given file"
  (not (null? (filter (lambda (type) (string-suffix? (symbol->string type) filename))
                 (asset-manager-file-types manager)))))

(define (asset-manager-load manager filename)
  "Calls the manager's load procedure with the given filename"
  (let ((dict (asset-manager-dict manager)))
    (if (hash-get-handle dict filename)
        (hash-ref dict filename)
        (hash-set! dict filename ((asset-manager-load-proc manager) filename)))))

(define asset-managers '())

(define (register-asset-manager types load-proc)
  "Creates and stores a new asset manager that handles the given file types"
  (set! asset-managers (cons (make-asset-manager types load-proc (make-hash-table)) asset-managers)))

(define (load-asset filename)
  "Loads the asset with the given filename. Returns #f if file type is not supported."
  (let loop ((managers asset-managers))
    (if (null? managers)
        #f
        (begin
          (let ((manager (car managers)))
            (if (asset-manager-match-type? manager filename)
                (asset-manager-load manager filename)
                (loop (cdr managers))))))))

;; Asset types
(register-asset-manager '(png jpg) load-image)
(register-asset-manager '(wav ogg flac) load-sample)
