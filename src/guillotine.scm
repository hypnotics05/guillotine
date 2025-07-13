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
             (ice-9 ftw)
             (ice-9 match))

#| 
  TODO: Test guillotine function
  TODO: Create cli
  TODO: Create better error handling
    What happens when a bad file is passed?
    When an error occurs during a video split?
  TODO: Pre-process
    If a single param is passed then assume this param is a timestamp file and processe it,
    outputing all (video . (start end)) into a file, also allow for timestamps to be created from 
    this file.
  TODO: Multi-thread video processing
  TODO: Bad timeline warnings
    When there are some unballanced IN and OUT, output a warning + timestamps and surrounding time stamps.
|#

(define clip-count 0)
(define video-count 0)
(define SHA1 60) ;; 1 minute in seconds
(define SHA5 300) ;; 5 minutes in seconds
(define SHA10 600) ;; 10 minutes in seconds
(define SHA30 30) ;; 30 seconds

;; Defines paris of (CMDS . epoch) inside of a list.
(define (string->cmd/epoch input)
  (drop-right 
    ;; HACK: There seems to be a final newline in normal and portal, may have to think of better 
    ;; solution if this fix causes issues
    (map (lambda (line)
           (let ((line-pair (string-split line #\space)))
             (cons (car line-pair) (string->number (last line-pair)))))
         (string-split input #\newline)) 
    1))

#|
  Creates a list of (START . END) pairs from epoch-pairs
    REC_START: time stamp is saved as the offset
    REC_END: used if there are no matching OUT to an IN
    IN & OUT: used as pairs, the IN timestamp is used to set the START, END is set using the next found OUT timestamp
    SHA1 START is timestamp - 1 minute or REC_START, END is the timestamp
    SHA5: START is timestamp - 5 minutes or REC_START, END is the timestamp
    SHA10: START is timestamp - 10 minutes or REC_START, END is the timestamp
    SHA30: START is timestamp - 30 seconds or REC_START, END is the timestamp
|#
(define (cmd/epoch->start/end cmds)
  (reverse 
    (let* ((REC_START (cdar cmds))
           (REC_END (- (cdr (last cmds)) REC_START))) ;; timestamp for the end of REC
      (let loop ((rest cmds) (result '()))
      (match rest
             (()
              result) ; return
             ((("IN" . time) . tail)
              ;; look for the next OUT
              (let ((out-pair (find (lambda (pair) (string=? (car pair) "OUT")) tail)))
                (if out-pair
                  (loop
                    ;; remove the OUT we matched
                    (delete out-pair tail)
                    (let ((END (- (cdr out-pair) REC_START)))
                      (cons (cons (- time REC_START) (if (> END REC_END) REC_END END)) 
                            result))) ; Create time pair
                  (loop tail result)))) ; no OUT found
             ((("SHA5" . time) . tail)
              (loop tail 
                    (let ((SHA (- time SHA5)))
                      (cons (cons (if (> REC_START SHA) 0 (- SHA REC_START)) (- time REC_START))
                            result))))
             ((("SHA10" . time) . tail)
              (loop tail 
                    (let ((SHA (- time SHA10)))
                      (cons (cons (if (> REC_START SHA) 0 (- SHA REC_START)) (- time REC_START)) 
                            result))))
             ((("SHA1" . time) . tail)
              (loop tail 
                    (let ((SHA (- time SHA1)))
                      (cons (cons (if (> REC_START SHA) 0 (- SHA REC_START)) (- time REC_START)) 
                            result))))
             ((("SHA30" . time) . tail)
              (loop tail 
                    (let ((SHA (- time SHA30)))
                      (cons (cons (if (> REC_START SHA) 0 (- SHA REC_START)) (- time REC_START)) 
                            result))))
             ((other . tail)
              (loop tail result)))))))

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
                           (string=? (caar rest) "REC_START")
                           (number? (cdar rest)))
                      ;; Found start of new range
                      (loop (cdr rest) #t (list (car rest)) acc))

                     ((and in-range?
                           (string=? (caar rest) "REC_END")
                           (number? (cdar rest)))
                      ;; End of current range, add it to result
                      (loop (cdr rest) #f '() (cons (reverse (cons (car rest) current)) acc)))

                     (in-range?
                       ;; Collect intermediate element
                       (loop (cdr rest) #t (cons (car rest) current) acc))

                     (else
                       ;; Outside of any range, keep scanning
                       (loop (cdr rest) #f current acc))))))
    (map (lambda (range) ;; 
           (cons (make-time time-utc 0 (cdar (last-pair range)))
                 range))
         ranges)))

#| FIX: logic errors
    First filter out list to only have mkv files
    Make sure path is being handle (It must end with /)
    System* error handling, we may not be appending, but overwritting
|#
(define (guillotine path time)
  (for-each 
    (lambda (pair) 
      (let* ((file 
               (find (lambda (file) 
                       (or (string-contains file (date->string (time-utc->date (car time)) 
                                                        "~Y-~m-~d-~H-~M-~S"))
                           (string-contains file 
                                     (date->string (time-utc->date 
                                                     (add-duration (car time) 
                                                                   (make-time time-duration 0 1))) 
                                                   "~Y-~m-~d-~H-~M-~S"))))
                     (scandir path)))
             (video (if file
                      (string-append path file)
                      (error "No matching video file found for " (car time))))
             (extension (string-append video ".mkv"))
             (range (cdr time)))
        (for-each 
          (lambda (clip)
            (let ((code (system* "ffmpeg"
                                 "-ss" (car clip)
                                 "-to" (cadr clip)
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
    (filter (lambda (pair)
              (not (null? (cdr pair))))
            (map (lambda (pair) 
                   (cons (car pair) (cmd/epoch->start/end (cdr pair)))) 
                 (string->video/slice time)))))

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
