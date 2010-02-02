#|
enter (run-stroop-model  :log-file t :num-trials n) to run a model and create a log file of the task execution with n trials .
enter (run-stroop-human  :log-file t :num-trials n) to run a human and create a log file of the task execution with n trials .

:log-file t == create log-file default is nil, do not create a log file
:num-trials n number of trials (n should be a multiple of 4) the default is 20


the log file is named stroop-xx where xx is a random number between 0 -99. the file is in th place indicated in the *file-name* variable
the file is close when the done message appears or the window is closed

Entries in the log file look like this 
Action TIME TRIAL-NUM TRIAL-TYPE WORD COLOR

TRIAL-TYPE
0 - color and word match, task is say word
1 - color and word dont match, task is say word
2 - color and word match, task is say color
3 - color and word dont match, task is say color

Example
CLICK-START	31883	1	3	"ORANGE"	:BLUE	
CLICK	32693	2	1	"BLACK"	:GREEN	
.....
CLICK	43588	24	3	"BLUE"	:ORANGE	
CLICK-DONE	44131	

|#

(defparameter *file-name* "~/")

(defparameter *act-r-run-time* 60)

(defun permute-a-list (lis)
  "Return a random permutation of the list"
  (do* ((item (nth (random (length lis)) lis) (nth (random (length temp)) temp))
        (temp (remove item lis :count 1) (remove item temp :count 1))
        (result (list item) (cons item result)))
       ((null temp) result)))

(defun make-trial-list (n)
  (let ((num (floor n 4)) (res nil))
    (dotimes (j num)
      (dotimes (i 4) 
        (push i res)))
    (permute-a-list res)))

(capi:define-interface stroop ()
  (
  
   (pen-position :initform (vector 0 0) :initarg :pen-position :accessor pen-position)
   (eye :initform nil :accessor eye)
   (ring :initform nil  :accessor ring)

   (current-item :initform nil :accessor current-item)
   (color-list :initform '(:green :red :yellow :black :blue :violet :orange) :accessor color-list)
   (current-idx :initform 0 :accessor current-idx)
   (log-p :initform nil :accessor log-p :initarg :log-p)
   (trial-type :initform (permute-a-list '(0 0 0 0 0 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3)) :accessor trial-type :initarg :trial-type)
   (subject-id :initarg :subject-id :accessor subject-id  :initform (random 100))
   (fs :initarg :fs :accessor fs :initform nil))
  (:panes
   (word-display capi:title-pane :text "" :accessor word-display :visible-border t :visible-min-width 100 :x 20 :y 40
                 :font (gp:make-font-description :family "Lucida Grande" :size 16.0 :weight :REGULAR :SLANT :ROMAN :PITCH :VARIABLE))
   (next-btn capi:push-button :accessor next-btn :callback 'stroop-task :text "Start" :x 20 :y 80))
   
  (:layouts
   (pinboard capi:pinboard-layout  '( word-display next-btn )
    :draw-pinboard-objects :local-buffer :accessor pinboard))
  (:default-initargs :title "Stroop task"
    :destroy-callback 'done-stroop
    :layout 'pinboard
    :best-x 400
    :best-y 400
    :best-width 200
    :best-height 200))

(defmethod done-stroop ((interface stroop))
  (when (fs interface)
      (close (fs interface))
      (setf (fs interface) nil)))

(defmethod log-event ((win stroop) event &rest data)
  (with-slots (fs subject-id) win
    (when (null fs)
      (setf fs (open (concatenate 'string *file-name* "stroop-" (write-to-string subject-id)) :direction :output)))
    (write event :stream fs) (write-char #\tab fs)
    (write (get-internal-real-time) :stream fs)  (write-char #\tab fs)
    (dolist (val data)
      (write val :stream fs)
      (write-char #\tab fs))
    (write-char #\newline fs)))

(defmethod stroop-task (data (interface stroop))
  (with-slots (current-item color-list current-idx trial-type next-btn word-display log-p) interface
    (cond ((< current-idx  (length trial-type))
           (setf current-item (nth (random (length color-list)) color-list))
           (setf (capi:title-pane-text word-display)  (remove #\: (write-to-string current-item)))
           (if (evenp (nth current-idx trial-type))
               (setf (capi:simple-pane-foreground word-display) current-item)
             (setf (capi:simple-pane-foreground word-display) (nth (random (1- (length color-list))) (remove current-item color-list))))
           (if (< (nth current-idx trial-type) 2)
               (setf (capi:item-text next-btn) "Say word")
             (setf (capi:item-text next-btn) "Say color"))
           (if log-p (log-event interface (if (zerop current-idx) 'click-start 'click) (1+ current-idx) (nth current-idx trial-type) (capi:title-pane-text word-display) 
                                (capi:simple-pane-foreground word-display)))
           (incf current-idx))
          (t
           (if log-p (log-event interface 'click-done ))
           (capi:display-message "Stroop task finished")
           (done-stroop interface)))))

(defun run-stroop-human (&key (log-file nil) (num-trials 20))
  (run-stroop :log-file log-file :model nil :num-trials num-trials))

(defun run-stroop-model (&key (log-file nil) (num-trials 20))
  (run-stroop :log-file log-file :model t :num-trials num-trials))

(defun run-stroop (&key (log-file t) (model t) (num-trials 20))
  (let ((win (make-instance 'stroop :log-p log-file :trial-type (make-trial-list num-trials))))
    (capi:display win)
#+ACT-R-6.0
    (when model
      (install-device win)
      (add-screen-object (word-display win) (get-vision))
      (add-screen-object (next-btn win) (get-vision))
      (print-visicon)
      (run *act-r-run-time* :real-time t)
      ;(mp:process-run-function "actr" '() #'run-act 60  t *standard-output*))
    )))

#+ACT-R-6.0 
(progn
(defun get-device () (device (get-module :device)))

(defun get-vision ()
  (get-module :vision))

(defmethod get-border-height ((dev capi:interface))
  (+
  (if (member :borderless (capi::simple-pane-window-styles dev)) 0 
#+:mac 22
#+:win32 22
    )
#+:mac 22 ;height of menubar
#+:win32 9
   ))

(defmethod build-vis-locs-for :around ((self capi:title-pane) ;;static-text-dialog-item
                               (vis-mod vision-module))
  (let ((chunks (call-next-method)))
    (dolist (c chunks) 
      
      (set-chunk-slot-value-fct c 'color (read-from-string (symbol-name (capi:simple-pane-foreground self)))))
    chunks))

#|
(defmethod build-vis-locs-for ((dev stroop) (vis-mod vision-module))
  (let ((res nil))
    (dolist (c )
      (push (make-feature-for c) res))
    res)) 
|#
(defun run-act (tm rt output)
  (setq *standard-output* output)
  (run tm :real-time rt))

(defmethod stroop-task :after (data (win stroop))
  (when (device (current-device-interface))
  (delete-screen-object (word-display win) (get-vision))
  (delete-screen-object (next-btn win) (get-vision))
  (add-screen-object (word-display win) (get-vision))
  (add-screen-object (next-btn win) (get-vision)) 
  (print-visicon)))

(clear-all)
(define-model stroop
;;       put set global parameters here ;;!!!!!!!!!!!!!!
(sgp 	:v t :esc t )
(sgp :show-focus t)
(hand-to-mouse (get-module :motor))

;;       put chunk types here           ;;!!!!!!!!!!!!!!
(chunk-type stroop state)


(add-dm
;;       put declarative memory here    ;;!!!!!!!!!!!!!!
(goal isa stroop state start)
)

(goal-focus goal)

;;       put productions here           ;;!!!!!!!!!!!!!!
(p test1 
 =goal> isa stroop state start
==>
 +visual-location> isa visual-location
  kind oval
 =goal> state next)

(p test2
 =goal> isa stroop state next
 =visual-location> isa visual-location
 ?visual> state free
 ?manual> state free
==> 
 +visual> isa move-attention
  screen-pos =visual-location
 +manual> isa move-cursor
  loc =visual-location
 =goal> state next1)

(p test3
 =goal> isa stroop state next1
 =visual> isa visual-object

  value =text
 ?manual> state free
==>
!output! (=text)
 +manual> isa click-mouse
 =goal> state next2)

(p test4
 =goal> isa stroop state next2
 ?manual> state free
==>
 +visual-location> isa visual-location
 screen-y highest
 =goal> state next3)


(p test5
 =goal> isa stroop state next3
 =visual-location> isa visual-location
 ?visual> state free
==> 
 +visual> isa move-attention
  screen-pos =visual-location
 =goal> state next4)



(p test6
 =goal> isa stroop state next4
 =visual> isa visual-object
==>
 +manual> isa click-mouse
 =goal> state next5)




; initialization
(setf *actr-enabled-p* t)
;;put goal-focus if needed here ;;!!!!!!!!!!!!!!
;;put set-production-parameters if needed here ;;!!!!!!!!!!!!!! 

 
) ;;end of model
) ;;end of act-r stuff

