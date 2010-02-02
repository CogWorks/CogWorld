(defvar *responses* nil)
(defvar *show-responses* nil)
(defvar *done* nil)

(defconstant *sperling-exp-data* '(3.03 2.40 2.03 1.50))

(defun do-trial (onset-time)
  
  (reset)
  
  (let* ((lis (permute-list '("B" "C" "D" "F" "G" "H" "J" 
                              "K" "L" "M" "N" "P" "Q" "R" 
                              "S" "T" "V" "W" "X" "Y" "Z")))
         (answers nil)   
         (tone (act-r-random 3))
         (window (open-exp-window "Sperling Experiment"                                      
                                  :visible t ;nil
                                  :width 300
                                  :height 300)))
    
    (dotimes (i 3)
      (dotimes (j 4)        
        (let ((txt (nth (+ j (* i 4)) lis)))
          (when (= i tone)
            (push txt answers))
          (add-text-to-exp-window :text txt
                                  :width 40
                                  :x (+ 75 (* j 50))
                                  :y (+ 101 (* i 50))))))
 

    (if *actr-enabled-p* 
        (progn
          
          (install-device window)
          (new-tone-sound (case tone (0 2000) (1 1000) (2 500)) .5 onset-time)
          ;(if (fboundp 'beep) 
          ;  (dotimes (i (1+ tone)) (beep))) 
          (schedule-event-relative (+ .9 (act-r-random .2)) 'clear-screen)
    
          (proc-display)
          (setf *responses* nil)
          (set-buffer-chunk 'imaginal 'letters)
          (run 30 :real-time t)) ;;;
      (progn
        (sleep onset-time) 
        
        (if (fboundp 'beep) 
            (dotimes (i (1+ tone)) (beep)) 
          (format t "Cannot generate sounds.~%Recall row ~S~%" (1+ tone)))
        
        (sleep (- 1.0 onset-time))
        
        (setf *done* nil)
        (setf *responses* nil)
        (clear-exp-window)
    
        (while (null *done*)
          (allow-event-manager window))))
    
    (when *show-responses*
      (format t "~%~%answers: ~S~%responses: ~S~%" answers *responses*))
    
    (compute-score answers)))


(defun compute-score (answers)
  (let ((score 0))
    (dolist (x answers score)
      (when (member x *responses* :test #'string-equal)
        (incf score)))))
    
  
(defun clear-screen () 
  (clear-exp-window)
  (proc-display))
   


(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (if (string= key " ")
      (setf *done* t)
    (push (string key) *responses*)))



(defun report-data (data)
  (correlation data *sperling-exp-data*)
  (mean-deviation data *sperling-exp-data*)
  (print-results data))

(defun print-results (data)
  (format t "~%Condition    Current Participant   Original Experiment~%")
  (do ((condition '(0.00 0.15 0.30 1.00) (cdr condition))
       (temp1 data (cdr temp1))
       (temp2 *sperling-exp-data* (cdr temp2)))
      ((null temp1))
    (format t " ~4,2F sec.          ~6,2F                ~6,2F~%" 
              (car condition) (car temp1) (car temp2))))


(defun run-block ()
  (let ((times (permute-list '(0.0 .15 .30 1.0)))
        (result nil))
    (dolist (x times)
      (push (cons x (do-trial x)) result))
    (sort result #'< :key #'car)))


(defun repeat-experiment (n)
  (let ((results (list 0 0 0 0)))
    (dotimes (i n)
      (setf results (mapcar #'+ results (mapcar #'cdr (run-block)))))
    (report-data (mapcar #'(lambda (x) (/ x n)) results))))

(clear-all)

(define-model sperling

(sgp :v t :show-focus t)
(sgp :declarative-finst-span 10)
(sgp :imaginal-delay 0)
(sgp  :needs-mouse nil :trace-detail medium)

(sgp :seed (100 0))

(chunk-type sperling-task state)
(chunk-type read-letters location tone  (upper-y 0) (lower-y 300))
(chunk-type report-row row)

(add-dm
 (attending isa chunk) (low isa chunk)
 (medium isa chunk) (high isa chunk)
 (find isa chunk) (encode isa chunk)
 (letters isa read-letters)
 (goal isa sperling-task state attending) )

(p detected-sound
   =aural-location>
     isa      audio-event
  
   ?aural>
      state    free
   
   ==>
   !output! (detected a sound)
   +aural>
     isa      sound
     event    =aural-location)

(p sound-respond-low
   =imaginal>
     isa      read-letters
     tone     nil
   =aural>
     isa      sound
     content  500
==>
    !output! (detected a low sound)
   =imaginal> 
     tone     low
     upper-y  205
     lower-y  215)

(p sound-respond-medium
   =imaginal>
     isa      read-letters
     tone     nil
   =aural>
     isa      sound
     content  1000
==>
   !output! (detected a medium sound)
   =imaginal>
     tone     medium
     upper-y  155
     lower-y  165)

(p sound-respond-high
   =imaginal>
     isa      read-letters
     tone     nil
   =aural>
     isa      sound
     content  2000
==>
  !output! (detected a high sound)
   =imaginal>
     tone     high
     upper-y  105
     lower-y  115)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(p attend-low
   =goal>
     isa      sperling-task
     state    attending
   =imaginal> isa read-letters
    upper-y =u lower-y =l 
   =visual-location>
     isa      visual-location
   > screen-y 204
   < screen-y 216
   ?visual> state    free 
==>
   !output! (Attending lowest row upper =u lower =l)
   =goal> state    encode
   =imaginal> location low
   +visual>
     isa      move-attention
     screen-pos =visual-location)

(p attend-medium
   =goal>
     isa      sperling-task
     state    attending
   =imaginal> isa read-letters
    upper-y =u lower-y =l 
   =visual-location>
     isa      visual-location
   > screen-y 154
   < screen-y 166
   ?visual> state    free
==>
   !output! (Attending middle row upper =u lower =l)
   =goal> state    encode
   =imaginal> location medium
   +visual>
     isa      move-attention
     screen-pos =visual-location)


(p attend-high
   =goal>
     isa      sperling-task  
     state    attending
   =imaginal> isa read-letters
   upper-y =u lower-y =l 
   =visual-location>
     isa      visual-location
   > screen-y 104
   < screen-y 116
   
   ?visual>
      state    free
==>
   !output! (Attending highest row upper =u lower =l)
   =goal> state    encode
   =imaginal> location high
   +visual>
     isa      move-attention
     screen-pos =visual-location)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(p encode-row-and-find
   =goal>
     isa      sperling-task state encode
   =imaginal> isa read-letters
     location =pos
     upper-y  =uy
     lower-y  =ly
   =visual>
     isa      text
     value =v
==>
   !output! (encoding =v at location =pos upper =uy lower =ly)
   =visual>
     status   =pos
   
   -visual>
   
   =imaginal> location nil
   =goal> state    attending
   +visual-location>
     isa      visual-location
     :attended nil
   > screen-y =uy 
   < screen-y =ly)

(P start-report
   =imaginal>
     isa      read-letters
     tone     =tone
   
   ?visual>
      state   free
   ==>
   !output! (starting a report)
   +goal>
     isa      report-row
     row      =tone
   +retrieval>
     isa      text
     status   =tone)

(P do-report
   =goal>
     isa      report-row
     row      =tone
   =retrieval>
     isa      text
     status   =tone
     value    =val
   
   ?manual>
      state    free
   ==>
   !output! (reporting =tone =val) 
   +manual>              
     isa      press-key     
     key      =val
   +retrieval>
     isa      text
     status   =tone
     :recently-retrieved nil
)

(p stop-report 
   =goal>
     isa      report-row
     row      =row
   
   ?retrieval>
      state   error
   
   ?manual>
      state    free
==>
   !output! (can not remember any more)
   +manual>              
     isa      press-key       
     key      space
   -goal>)

(setf *actr-enabled-p* t)

(setf *show-responses* t)

(goal-focus goal)

(spp start-report :u -2)
(spp detected-sound  :u 10)
(spp sound-respond-low :u 10)
(spp sound-respond-medium :u 10)
(spp sound-respond-high :u 10)

)
