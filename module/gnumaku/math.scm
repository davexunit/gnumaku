(define-module (gnumaku math)
  #:export (pi deg2rad rad2deg cos-deg sin-deg atan-deg))

(define pi 3.141592654)

(define (deg2rad angle)
  (* angle (/ pi 180)))

(define (rad2deg angle)
  (* angle (/ 180 pi)))

(define (cos-deg angle)
  (cos (deg2rad angle)))

(define (sin-deg angle)
  (sin (deg2rad angle)))

(define (atan-deg y x)
  (rad2deg (atan y x)))
