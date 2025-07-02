#! /usr/bin/env -S guile -s
!#

(use-modules (srfi srfi-64)
             (srfi srfi-19)
             (ice-9 ports)
             (ice-9 textual-ports))
; (ice-9 match))

(load "src/guillotine.scm")

(define (file->string filename)
  (call-with-input-file filename
                        (lambda (port)
                          (get-string-all port))))

(define data "")
(define cmd/epoch '())
(define start/end '())
(define video/slice '())

;; Testing a real user file
(test-begin "Portal" 3)

(set! data (file->string "tests/portal"))

(set! cmd/epoch 
  (list (cons "OUT" 1751405388) (cons "REC_START"  1751407127) 
        (cons "IN"  1751407241) (cons "OUT"  1751407350) (cons "IN"  1751407380) 
        (cons "OUT"  1751407405) (cons "SHA5"  1751407417) (cons "IN"  1751407421) 
        (cons "OUT"  1751407501) (cons "SHA5"  1751407582) (cons "IN"  1751407621) 
        (cons "OUT"  1751407659) (cons "IN"  1751407672) (cons "OUT"  1751407728) 
        (cons "OUT"  1751407836) (cons "SHA5"  1751407891) (cons "IN"  1751407897) 
        (cons "OUT"  1751407931) (cons "IN"  1751407970) (cons "OUT"  1751408076) 
        (cons "IN"  1751411039) (cons "OUT"  1751411260) (cons "IN"  1751411314) 
        (cons "OUT"  1751411618) (cons "IN"  1751411644) (cons "OUT"  1751411836) 
        (cons "REC_END"  1751411859)))

(set! start/end
  (list (cons 114 223) (cons 253 278) (cons 0 290) (cons 294 374) (cons 155 455) 
        (cons 494 532) (cons 545 601) (cons 464 764) (cons 770 804) 
        (cons 843 949) (cons 3912 4133) (cons 4187 4491) (cons 4517 4709)))

(set! video/slice
  (list (cons (make-time time-utc 0 1751411859)
              (list (list (cons "REC_START" 1751407127) (cons "IN" 1751407241) (cons "OUT" 1751407350) 
                          (cons "IN" 1751407380) (cons "OUT" 1751407405) (cons "SHA5" 1751407417) 
                          (cons "IN" 1751407421) (cons "OUT" 1751407501) (cons "SHA5" 1751407582) 
                          (cons "IN" 1751407621) (cons "OUT" 1751407659) (cons "IN" 1751407672) 
                          (cons "OUT" 1751407728) (cons "OUT" 1751407836) (cons "SHA5" 1751407891) 
                          (cons "IN" 1751407897) (cons "OUT" 1751407931) (cons "IN" 1751407970) 
                          (cons "OUT" 1751408076) (cons "IN" 1751411039) (cons "OUT" 1751411260) 
                          (cons "IN" 1751411314) (cons "OUT" 1751411618) (cons "IN" 1751411644) 
                          (cons "OUT" 1751411836) (cons "REC_END" 1751411859))))))

(test-equal "File -> CMD/EPOCH"
            cmd/epoch
            (string->cmd/epoch data))

(test-equal "CMD/EPOCH -> START/END"
            start/end
            ;; in string->video/slice all cmd/epoch's start with REC_START & end with REC_END
            ;; But the portal test has a stray OUT in the beginning
            (cmd/epoch->start/end (cdr cmd/epoch))) 

(test-equal "File -> VIDEO/SLICE"
            video/slice
            (string->video/slice data))

(test-end "Portal")

(test-begin "Normal" 3)

(set! data (file->string "tests/normal"))

(set! cmd/epoch 
  (list 
    (cons "REC_START" 1751394136) (cons "IN" 1751394194) (cons "IN" 1751394243) 
    (cons "OUT" 1751394266) (cons "IN" 1751394272) (cons "OUT" 1751394276) 
    (cons "OUT" 1751394300) (cons "SHA5" 1751397522) (cons "IN" 1751397524) 
    (cons "OUT" 1751397737) (cons "SHA10" 1751400501) (cons "IN" 1751400511) 
    (cons "OUT" 1751400519) (cons "REC_END" 1751400568))) 

(set! start/end
  (list 
    (cons 58 130) (cons 107 140) (cons 136 164) (cons 3086 3386) 
    (cons 3388 3601) (cons 5765 6365) (cons 6375 6383)))

(set! video/slice 
  (list (cons (make-time time-utc 0 1751400568)
              (list (list (cons "REC_START" 1751394136) (cons "IN" 1751394194) (cons "IN" 1751394243) 
                          (cons "OUT" 1751394266) (cons "IN" 1751394272) (cons "OUT" 1751394276) 
                          (cons "OUT" 1751394300) (cons "SHA5" 1751397522) (cons "IN" 1751397524) 
                          (cons "OUT" 1751397737) (cons "SHA10" 1751400501) (cons "IN" 1751400511) 
                          (cons "OUT" 1751400519) (cons "REC_END" 1751400568))))))

(test-equal "File -> CMD/EPOCH"
            cmd/epoch
            (string->cmd/epoch data))

(test-equal "CMD/EPOCH -> START/END"
            start/end
            (cmd/epoch->start/end cmd/epoch))

(test-equal "File -> VIDEO/SLICE"
            video/slice
            (string->video/slice data))
(test-end "Normal")

(test-begin "Edge" 0)
(test-end "Edge")

(test-begin "Regression" 0)
(test-end "Regression")
