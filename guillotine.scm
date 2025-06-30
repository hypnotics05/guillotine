#! /usr/bin/env -S guile -e main -s
!#

(use-modules (ice-9 format)
             (ice-9 match))

(define elog "")
(define clip-count 0)
(define video-count 0)
(define SHA5 300) ;; 5 minutes in seconds
(define SHA10 600) ;; 10 minutes in seconds

#|
  Creates a list of (START . END) pairs from input file:
    REC_START: time stamp is saved as the offset
    REC_END: used if there are no matching OUT to an IN
    IN & OUT: used as pairs, the IN timestamp is used to set the START, END is set using the next found OUT timestamp
    SHA5: START is timestamp - 5 minutes or REC_START, END is the timestamp
    SHA10: START is timestamp - 10 minutes or REC_START, END is the timestamp
|#
(define (clip-factory input)
  (let* ((cmds (map (lambda (line)
                      ;; Creates a list of pairs (CMD . epoch)
                      (let ((line-pair (string-split line #\space)))
                        (cons (car line-pair) (string->number (car (cdr line-pair))))))
                    (string-split input #\newline)))
         (REC_START (cdr (car cmds)))
         (REC_END (- (cdr (car (list-tail cmds (- (length cmds) 1)))) REC_START))) ;; timestamp for the end of REC
    (let loop ((rest cmds) (result '()))
      (match rest
             (()
              result) ; return
             ((("IN" . in-time) . tail)
              ;; look for the next OUT
              (let ((out-pair (find (lambda (pair) (string=? (car pair) "OUT")) tail)))
                (if out-pair
                  (loop
                    ;; remove the OUT we matched
                    (delete out-pair tail)
                    (let ((END (- (cdr out-pair) REC_START)))
                      (cons (list (- in-time REC_START) (if (> END REC_END) REC_END END)) 
                            result))) ; Create time pair
                  (loop tail result)))) ; no OUT found
             ((("SHA5" . S5-time) . tail)
              (loop tail 
                    (let ((SHA (- S5-time SHA5)))
                    (cons (list (if (> REC_START SHA) 0 (- SHA REC_START)) S5-time) 
                          result))))
             ((("SHA10" . S10-time) . tail)
              (loop tail 
                    (let ((SHA (- S10-time SHA10)))
                    (cons (list (if (> REC_START SHA) 0 (- SHA REC_START)) S10-time) 
                          result))))
             ((other . tail)
              (loop tail result))))))

;; Creates the video clips, if any errors creates a log file with (string-null?)
(define (guillotine video time)
  (for-each 
    (lambda (pair)
      (display pair)
      (newline))
    time))

#|
  guillotine path [video time]
    path: the path where all file are contained
    [video time]: a repeating pattern of a video file and a timestamp file.
|#
(define (main args)
  (let ((path (cadr args)))
    (map (lambda (video time)
           (guillotine video (clip-factory time)))
         (cddr args)))
  ;; Should display this message and the file name of the error log file generated if applicable
  (display (format #t "Finished processing ~d clips from ~d videos" clip-count video-count))
  (newline))
