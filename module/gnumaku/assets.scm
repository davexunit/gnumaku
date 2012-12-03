(define-module (gnumaku assets)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (gnumaku core)
  #:export (load-asset register-asset-manager))

(define asset-managers '())

(define asset-directory "assets")

(define-record-type <asset-manager>
  (make-asset-manager directory load-proc dict)
  asset-manager?
  (directory asset-manager-directory)
  (load-proc asset-manager-load-proc)
  (dict asset-manager-dict))

(define (asset-manager-build-path manager filename)
  "Builds path to asset using the asset manager's directory"
  (string-append asset-directory "/" (asset-manager-directory manager) "/" filename))

(define (asset-manager-load manager filename . args)
  "Calls the manager's load procedure with the given filename"
  (let ((asset (apply asset-manager-fetch manager filename args)))
    (or asset (apply asset-manager-load-new manager filename args))))

(define (memo-match? pair args)
  "Checks for equivalence between the arguments used to load the asset in `pair` and the arguments passed in"
  (equal? args (car pair)))

(define (asset-manager-fetch manager filename . args)
  "Fetches the asset loaded from `filename` with the correct arguments. Returns #f if not found."
  (if (hash-ref (asset-manager-dict manager) filename)
      (apply asset-manager-lookup manager filename args)
      #f))

(define (asset-manager-lookup manager filename . args)
  "Searchs memo with key of `filename` for the asset constructed with the same arguments.
   Returns #f if not found."
  (let ((memo (hash-ref (asset-manager-dict manager) filename)))
    (let ((asset-pair (find (lambda (elem) (memo-match? elem args)) memo)))
      (if (pair? asset-pair)
          (cdr asset-pair)
          #f))))

(define (asset-manager-load-new manager filename . args)
  "Loads a new asset, memoizes it, and inserts it into the dictionary"
  (let ((dict (asset-manager-dict manager)))
    ;; Initialize list if key doesn't exist in hash yet
    (unless (hash-get-handle dict filename)
      (hash-set! dict filename '()))
    (let ((memo (hash-ref dict filename)) ;; Fetch memo list
          (asset (apply (asset-manager-load-proc manager) filename args))) ;; Load asset
      ;; Add a new args/asset pair to the memo
      (hash-set! dict filename (cons (cons args asset) memo))
      ;; Return the asset
      asset)))

(define (register-asset-manager types load-proc)
  "Creates and stores a new asset manager that handles the given file types"
  (set! asset-managers (cons (make-asset-manager types load-proc (make-hash-table)) asset-managers)))

(define (load-asset filename . args)
  "Loads the asset with the given filename. Returns #f if file type is not supported."
  (let loop ((managers asset-managers))
    (if (null? managers)
        #f ;; No manager could load the file
        (begin
          (let ((manager (car managers)))
            (let ((filename (asset-manager-build-path manager filename)))
              (if (file-exists? filename)
                  (apply asset-manager-load manager filename args)
                  (loop (cdr managers)))))))))
