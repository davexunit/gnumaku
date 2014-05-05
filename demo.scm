#! /bin/sh
env guile -L ./module/ -s ./demo.scm
exit
!#

(use-modules (demo main))
(main)
