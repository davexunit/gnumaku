#! /usr/bin/guile -s
!#

;; Just a simple bootstrap script.

(add-to-load-path ".")
(use-modules (gnumaku main))
(gmk-main)
