#! /usr/bin/env -S guile -e main -s
!#


#|
  Pipeline:
    Open time file -> file goes to string->video/slice -> guillotine
    inside guillotine:
      for each member of video/slice parse slice into start/end, for every start end create a 
      clip of the video using start/end
|#

(use-modules (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-19)
             (ice-9 match))

#| 
  TODO: Time accuracy
    Since we are using timestamps to name files and relying purely on timing, we should be looking 
    for a video file whose name could be 1 second more then the provided name.
|#

(define clip-count 0)
(define video-count 0)
(define SHA5 300) ;; 5 minutes in seconds
(define SHA10 600) ;; 10 minutes in seconds

;; Defines paris of (CMDS . epoch) inside of a list.
(define (string->cmd/epoch input)
  (map (lambda (line)
         (let ((line-pair (string-split line #\space)))
           (cons (car line-pair) (string->number (car (cdr line-pair))))))
       (string-split input #\newline)))

#|
  Creates a list of (START . END) pairs from epoch-pairs
    REC_START: time stamp is saved as the offset
    REC_END: used if there are no matching OUT to an IN
    IN & OUT: used as pairs, the IN timestamp is used to set the START, END is set using the next found OUT timestamp
    SHA5: START is timestamp - 5 minutes or REC_START, END is the timestamp
    SHA10: START is timestamp - 10 minutes or REC_START, END is the timestamp
|#
(define (cmd/epoch->start/end cmds)
  (let* ((REC_START (cdr (car cmds))) ;; TODO: refactor to use last-pair 
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

#|
  Creates a list of (time . (epoch-pairs)) to be processed.
|#
(define (string->video/slice file)
  (let* ((cmds (string->cmd/epoch file))
         (ranges (let loop ((rest cmds)
                            (in-range? #f)
                            (current '())
                            (acc '()))
                   (cond
                     ((null? rest)
                      ;; If end of list and currently collecting, discard incomplete
                      (reverse acc))

                     ((and (not in-range?)
                           (string=? (car (car rest)) "REC_START")
                           (number? (cdr (car rest))))
                      ;; Found start of new range
                      (loop (cdr rest) #t (list (car rest)) acc))

                     ((and in-range?
                           (string=? (car (car rest)) "REC_END")
                           (number? (cdr (car rest))))
                      ;; End of current range, add it to result
                      (loop (cdr rest) #f '() (cons (reverse (cons (car rest) current)) acc)))

                     (in-range?
                       ;; Collect intermediate element
                       (loop (cdr rest) #t (cons (car rest) current) acc))

                     (else
                       ;; Outside of any range, keep scanning
                       (loop (cdr rest) #f current acc))))))
    (map (lambda (range) ;; 
           (cons (make-time time-utc 0 (cdr (car (last-pair range))))
                 (list range)))
         ranges)))

(define (guillotine path time)
  (for-each 
    (lambda (pair) 
      (let* ((file 
               (find (lambda (file) 
                       (or (string=? file (date->string (time-utc->date (car time)) "~Y-~m-~d-~H-~M-~S"))
                           (string=? file (date->string (time-utc->date (add-duration (car time) (make-time time-duration 0 1))) "~Y-~m-~d-~H-~M-~S"))))
                     (scandir path)))
             (video (if file
                      (string-append path file)
                      (error "No matching video file found for " (car time))))
             (extension (string-append video ".mkv"))
             (range (cmd/epoch->start/end (cdr time))))
        (for-each 
          (lambda (clip)
            (let ((code (system* "ffmpeg"
                                 "-ss" (car clip)
                                 "-to" (car (cdr clip))
                                 "-i" extension
                                 "-map" "0"
                                 "-c:v" "libx264"
                                 "-preset" "fast"
                                 "-crf" "18"
                                 "-c:a" "aac"
                                 "-b:a" "192k"
                                 "-ar" "48000"
                                 "-movflags" "+faststart"
                                 (string-append video "-" (number->string clip-count) ".mp4"))))
              (if (= code 0)
                (set! clip-count (+ clip-count 1)) ;; Video parsed successfully
                (call-with-output-file 
                  "guillotine-errors.log"
                  (lambda (port)
                    (display (string-append extension " failed with clip " clip) port))))))
          range)
        (set! video-count (+ video-count 1))))
    (string->video/slice time)))

#|
  guillotine path file
    path: the path containing all videos, videos must be of format "%Y-%m-%d-%H-%M-%S".mkv
    file: the file containing the list of commands and their epoch.
|#
(define (main args)
  (let ((path (cadr args)))
    (map (lambda (video time)
           (display video)
           (newline)
           (display time)
           (newline))
         (cddr args)))
  ;; Should display this message and the file name of the error log file generated if applicable
  (display (format #t "Finished processing ~d clips from ~d videos" clip-count video-count))
  (newline))
