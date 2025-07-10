#! /usr/bin/env -S guile -s
!#

(use-modules (srfi srfi-98)
             (ice-9 rdelim)
             (srfi srfi-1))

;; For each value in path display next to index, read user input, install to path of index provided by user

(define path (string-split (get-environment-variable "PATH") #\:))
(define script "src/guillotine.scm")

(let loop ((lst path) (i 0))
  (when (pair? lst)
    (format #t "~a) ~a~%" i (car lst))
    (loop (cdr lst) (+ i 1))))

(display "Install loc> ")
;; The Name of the directory to install guillotine to
(define dir (last (list-head path (+ (read) 1))))
(newline)

(define code (system* 
  "cp" script
  (string-append dir "guillotine")))

(unless (= code 0)
  (error "Failed to install guillotine with code " code)) 

(display "Installed guillotine to ")
(display dir)
(newline)
