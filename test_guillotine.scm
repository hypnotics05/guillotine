#! /usr/bin/env -S guile -s
!#

(use-modules (srfi srfi-64)
             (ice-9 match))

(load "guillotine.scm")

(define datam (open-input-file "tests/basic"))

(test-begin "Guillotine Input Parse Tests")

(test-end)
