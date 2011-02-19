;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename     : control.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Author       : Mike Schoelles, Chris R. Sims
;; Copyright    : (C) 2005, 2009 CogWorks Laboratory
;; Address      : Cognitive Science Department
;;              : Rennselaer Polytechnic Institute
;;              : Troy, NY 12180
;;              : schoem@rpi.edu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Version      : 2.2.0
;; Description  : Defines top-level control functions for starting & stopping
;;  the experiment, defining tasks, registering the subject, etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; History
;;
;; [2005.03.10] : File created
;; [2009.08.24] : Converted to Cogworld
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Task communication
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-task (name &key (run-function nil) (break-function nil)
                           (configure-function nil) (replay-function nil)
                           (model-run-function nil) (path nil) (app 'lisp))
  (let ((task (make-instance 'task-class
                             :name name
                             :configure-function configure-function
                             :run-function run-function
                             :model-run-function model-run-function
                             :break-function break-function
                             :replay-function replay-function
                             :path path
                             :app app)))
        
    (push task (task-list *cw*))
    task))

;; Provided for backwards compatability: Use REGISTER-TASK
(defun mw-register-task (name &key (run-fn nil) (break-fn nil) (configure-fn nil)
                              (model-run-fn nil))
  (register-task
   name
   :run-function run-fn
   :model-run-function model-run-fn
   :configure-function configure-fn
   :break-function break-fn
   :replay-function nil)
  T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experiment setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod run-remote-app ()
  (let ((tsk (nth (current-task *cw*) (task-list *cw*))))
    (when tsk
      (cond
       ((equal (app tsk) 'python)
        (change-directory (directory-namestring (namestring (path tsk))))
        (let ((cmd (format nil "~A ~A ~A~@[ ~A~]"
                           (capi:text-input-pane-text (python-binary (control-window *cw*)))
                           (namestring (path tsk))
                           (json-rpc-server-get-port)
                           (task-condition *cw*)
                           )))
          (asdf:run-shell-command cmd)
          (task-finished tsk)))
       ((equal (app tsk) 'unix)
        (asdf:run-shell-command (namestring (path tsk)))
        (task-finished tsk))
       ))))
           

(defmethod load-tasks ((cw cogworld))
  (let* ((task-panel (task-list (control-window cw)))
         (items (capi:collection-items task-panel)))
    (dotimes (i (length items))
      (let* ((tsk (aref items i))
             (fn (file-namestring tsk))
             (ftype (pathname-type fn)))
        (cond ((equal ftype (pathname-type (compile-file-pathname "foo.lisp")))
               (load tsk))
              ((equal ftype "lisp")
               (compile-file tsk :load t))
              ((equal ftype "m")
               (if (null (remote-app)) (make-remote-app))
               (register-task (subseq fn 0 (- (length fn) 2)) :run-function 'run-remote-app :app 'matlab :path tsk)
               (setf *use-matlab* (1+ *use-matlab*)))
              ((equal ftype "py")
               (register-task fn :run-function 'run-remote-app :app 'python :path tsk))
              (t ; If there is no file extension then assume its a unix application
               (if (null (remote-app)) (make-remote-app))
               (register-task fn :run-function 'run-remote-app :app 'unix :path tsk))
               )))
    (setf (task-list cw) (reverse (task-list cw)))
    (if (plusp *use-matlab*)
        (mp:process-run-function "Start MATLAB engine" nil 'start-matlab))
    (dolist (task (task-list cw))
      (log-info (list "CW-EVENT" "TASK-LOADED" (name task))))))

(defmethod configure-tasks ((cw cogworld))
  (setf (dispatched-configs cw) 0)
  (dolist (task (task-list cw))
    (if (configure-function task)
        (progn (incf (dispatched-configs cw))
          (apply (configure-function task) nil))))
  (setf (startup-process cw)  mp:*current-process*)
  (mp:process-wait
   "Waiting for task configs to return"
   #'(lambda ()
       (= (dispatched-configs cw) 0)))
  (setf (startup-process cw) nil))

(defun configuration-done (task-obj &key (condition "?"))
  (setf (task-condition task-obj) condition)
  (push condition (task-condition-list *cw*))
  (setf *task-conditions* (task-condition-list *cw*))
  (decf (dispatched-configs *cw*)))

;; Provided for backward compatability: use CONFIGURATION-DONE
(defun mw-configure-done (cond-string)
  (configuration-done (nth (1- (dispatched-configs *cw*)) (task-list *cw*))
                      :condition cond-string))

(defun task-conditions ()
  (task-condition-list *cw*))

(defmethod run-tasks-concurrently ((cw cogworld))
  (dolist (task (task-list cw))
    (mp:process-run-function
     (name task)
       nil
       #'(lambda (task)
           (apply (run-function task) nil))
       task)))

(defmethod run-tasks ((cw cogworld))
  (with-slots (current-task task-list) cw
    (while current-task
      (apply (run-function (nth (current-task cw) task-list)) nil)))
  (stop-experiment cw))

;; Provided for backward compatability: use TASK-FINISHED.
(defun mw-task-finished (name)
    (setf (task-list *cw*)
          (remove name (task-list *cw*) 
                  :test (lambda (string object) (equal string (name object)))))
    (cond 
     ((null (task-list *cw*))
      (stop-experiment *cw*))))

#|
(defun task-finished (task)
  ;(if (break-function task)
  ;    (apply (break-function task) nil))
  (setf (task-list *cw*) (remove task (task-list *cw*) :test 'equal))
  (if (null (task-list *cw*)) (stop-experiment *cw*)))
|#

(defmethod task-finished ((task task-class))
  (when (eql (app task) 'matlab)
    (capi:display-message "Press OK for next task"))
  (with-slots (current-task run-proc task-list) *cw*
    (set-mouse-position 10 40)
     (incf current-task)
     (if (>= current-task (length task-list)) (setf current-task nil))
     (mp:process-poke run-proc )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Experiment control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod start-experiment ((cw cogworld))
  (with-slots (status task-condition-list control-window subject-info control-mode current-task) cw
    (setf current-task 0)
    (setf status nil)
    (setf task-condition-list nil)
    (define-logging-folder (capi:text-input-pane-text (logging-folder (control-window cw))))
    (capi:apply-in-pane-process
     (button-start control-window )
     #'(lambda ()
         (setf (capi:item-text (button-start control-window )) "Stop")))
    (eeg-proc 'initialize (capi:text-input-pane-text (eeg-ip (control-window cw))))
    (setf subject-info  nil)
    (setf control-mode  :human)
    (start-experiment-human cw)
    (setf status  :running)
    (monitor-regions)))

(defmethod start-experiment-log-only ((cw cogworld))
  (with-slots (status control-window subject-info  experiment-name
               dispatched-configs listener-window background-window task-list) cw   
    (setf task-list  nil)
    (setf subject-info (make-instance 'subject-info-class))
    (setf experiment-name (capi:text-input-pane-text experiment-name control-window ))
    (setf dispatched-configs 0)
    (create-background-window)
    (show-background-window)
    (when (not (debug-p))
        (capi:execute-with-interface
         control-window
         #'(lambda ()
             (capi:hide-interface control-window nil))) 
        (if listener-window
            (capi:execute-with-interface
             listener-window 
             #'(lambda ()
                 (capi:hide-interface listener-window  nil)))))
    (define-logging-folder (capi:text-input-pane-text (logging-folder control-window )))
    (open-logging-file cw)
    (mp:process-wait "idle" (lambda (win) 
                                  (multiple-value-bind (x y w h) (capi:top-level-interface-geometry win)
                                    (and x y w h)))
                     background-window )                           
    (when (connect-eyetracker cw (capi:text-input-pane-text (eyetracker-ip (control-window cw))))
      (hide-background-window)
      (log-header)
      (capi:display (make-instance 'cw-toolbar))  
    )))

(defun restore-debug ()
  (show-menu-bar)
  (capi:display (make-instance 'LISPWORKS-TOOLS::LISPWORKS-TOOLBAR-WINDOW)))

(defun find-listener ()
  (find 'lispworks-tools:listener (capi:screen-interfaces (capi:convert-to-screen)) :key 'type-of))

(defmethod start-experiment-human ((cw cogworld))
  (with-slots (control-window task-list experiment-name dispatched-configs 
              listener-window background-window status run-proc) cw
    ;; Kill the LispWorks toolbar window
    (if (not *delivered*)
        (setf listener-window (find-listener))
        (dolist (interface (capi:screen-interfaces (capi:convert-to-screen)))
          (if (equal (type-of interface)
                     'LISPWORKS-TOOLS::LISPWORKS-TOOLBAR-WINDOW)
              (capi:destroy interface))))
    (setf task-list nil)
    (setf experiment-name (capi:text-input-pane-text (experiment-name control-window)))
    (setf dispatched-configs 0)
    ;; Load the tasks
    (load-tasks cw)

    (create-background-window)
    (show-background-window)
    (when (not (debug-p))
      (capi:execute-with-interface control-window
         #'(lambda () (capi:hide-interface control-window nil)))
      (if listener-window
          (capi:execute-with-interface listener-window
             #'(lambda () (capi:hide-interface listener-window nil)))))
    (configure-tasks cw)
    (register-subject cw)
    (when (capi:item-selected (check-logging control-window)) 
      (create-history-file cw)
      (open-logging-file cw))
    (when (not (eql status :halted))
      (if (debug-p) (hide-background-window))
      (log-header)
      (with-slots (first-name  last-name age major gender exp-history) (subject-info cw)
        (let ((header (list  "FIRST" "LAST" "AGE" "MAJOR" "GENDER" "RIN" "EXP-HISTORY"))
              (values (list first-name last-name age major gender (get-rin) exp-history)))
          (write-history-file cw header values)))
      (when (capi:item-selected (check-eyetracker control-window))     
        (mp:process-wait "idle" (lambda (win) 
                                  (multiple-value-bind (x y w h) (capi:top-level-interface-geometry win)
                                    (and x y w h))) background-window)
        (capi:apply-in-pane-process background-window #'capi:raise-interface background-window)
        (if (null (connect-eyetracker cw (capi:text-input-pane-text (eyetracker-ip (control-window cw))))) (setf (status cw) :halted)))
      (when (not (eql status :halted))
        (capi:display-message "Press OK to start the experiment.")
        (if (not (debug-p)) (capi:apply-in-pane-process  background-window #'capi:raise-interface background-window))
        (raise-tasks)
        (if (capi:item-selected (check-eyetracker control-window)) (start-eyetracking "BEGINNING-OF-TASK")))
      (if (not (eql status :halted)) 
          (setf run-proc (mp:process-run-function "run-task" '() #'run-tasks cw)) 
        (capi:display-message "CW :HALTED"))
    )))


(defun load-actr () 
  (let* ((dir (directory-namestring (cw-path *cw*)))
         (path1 (subseq dir 0 (- (length dir) (length "source/"))))  
        #+(and :lispworks :win32 (not :lispworks5)) (typ "fsl")
        #+(and :lispworks :win32 :lispworks5) (typ "ofasl")
        #+(and :lispworks :unix (not :macosx) (not :lispworks5)) (typ "ufsl")
        #+(and :lispworks :unix (not :macosx) :lispworks5)  (typ "ufasl")
        #+(and :lispworks :macosx (not :x86))  (typ "nfasl")
        #+(and :lispworks :macosx :x86) (typ "xfasl")
         (act-r-path (concatenate 'string path1 "actr6/load-act-r-6." typ))) 
    (load act-r-path :verbose t))
  (when (null (listener-window *cw*))
    (setf (listener-window *cw*) (make-instance 'listener-window))
    (capi:display (listener-window *cw*)))
  (setq *act-r-stream* (capi:interactive-pane-stream (listener (listener-window *cw*)))))


(defun explode-tab (string)
  (let ((items nil)
        (pos nil))
    (loop
     (when (= (length string) 0) (return))
     (setf pos (position #\tab string))
     (if pos
         (progn (push (subseq string 0 pos) items)
           (setf string (subseq string (1+ pos))))
       (progn (push string items)
         (setf string ""))))
    (reverse items)))

#|
(defun column-value (text column)
  (dotimes (i (1- column))
    (if (position #\tab text)
        (setf text (subseq text (1+ (position #\tab text))))
      (setf text "")
      ))
  (subseq text 0 (position #\tab text)))
|#

(defparameter *cw-transparent-red* (color:make-rgb 1.0 0.0 0.0 0.5))
(defparameter *cw-x-scale* 1);(/ 1280 1024))
(defparameter *cw-y-scale* 1);(/ 1024 768))

(defun draw-eg-event (pane event-list)
  (declare (type (fixnum ))
           (optimize (speed 3)))
  (let* ((gaze-x (* (read-from-string (nth 7 event-list)) *cw-x-scale*))
         (gaze-y (- (* (read-from-string (nth 8 event-list)) *cw-y-scale*) 30))
         (mouse-x (read-from-string (nth 10 event-list)))
         (mouse-y (- (read-from-string (nth 11 event-list)) 30))
         (button-state (read-from-string (nth 12 event-list)))
         (min-x (min gaze-x mouse-x))
         (min-y (min gaze-y mouse-y))
         (width (- (max gaze-x mouse-x) min-x))
         (height (- (max gaze-y mouse-y) min-y)))
  
  (gp:draw-circle pane gaze-x gaze-y 35 :foreground *cw-transparent-red* :filled t)
  (gp:draw-circle pane mouse-x mouse-y 5 :foreground :black :filled t)
  (if (not (= button-state 0))
      (gp:draw-string pane "(Click!)" (- mouse-x 23) (- mouse-y 5)))
  (values (- min-x 35) (- min-y 35) (+ width 75) (+ height 75))
  ))

(defmethod start-experiment-replay ((cw cogworld))
  (let* ((replay-file (capi:prompt-for-file "Please choose a log file:"
                                            :operation :open :filter "*.xls"))
         (replay-handle (open replay-file :direction :input))
         (eof nil)
         (start-time (get-internal-real-time))
         (data-start-time nil)
         (state-list nil)
         (replay-pane nil)
         (buffer-pane nil)
         (timestamp nil)
         (eg-list nil)
         (redraw-x 0)
         (redraw-y 0)
         (redraw-width 1280)
         (redraw-height 1024))

    (load-tasks cw)
    
    ;; Load log file data
    (loop
     (when eof (return))
     (multiple-value-bind (line-text line-terminal) (read-line replay-handle nil)
       (if line-terminal (setf eof t))
       (setf line-text (explode-tab line-text))
       (cond
        ((nth 7 line-text)
         (if (equal (read-from-string (nth 7 line-text)) "TASK-STATE")
             (push line-text state-list))))
       (cond
        ((nth 4 line-text)
         (if (equal (read-from-string (nth 4 line-text)) "EG-DATA")
             (push line-text eg-list))))))
    (capi:display-message "~a" (length state-list))

    (setf state-list (reverse state-list))
    (setf eg-list (reverse eg-list))

    (setf replay-pane (make-instance 'capi:output-pane :visible-min-width 1280 :visible-min-height 1024))
    (capi:display
     (make-instance 'capi:interface
                    :layout (make-instance 'capi:simple-layout :description (list replay-pane))
                    :x 0 :y 0))
    (capi:with-geometry replay-pane
      (setf buffer-pane (gp:create-pixmap-port replay-pane capi:%width% capi:%height% :background :white :clear t)))
    ;; CRS
    (cond
     ((and (nth 2 (first state-list))
           (nth 2 (first eg-list)))
      (setf timestamp (min (read-from-string (nth 2 (first state-list)))
                           (read-from-string (nth 2 (first eg-list)))))))
    (setf data-start-time timestamp)
    (apply (replay-function (first (task-list *cw*))) (list buffer-pane (first state-list)))
    (setf start-time (get-internal-real-time))

    (dotimes (i (+ (length state-list) (length eg-list)))
      (let ((next-state-timestamp (if state-list (read-from-string (nth 2 (first state-list)))))
            (next-eg-timestamp (if eg-list (read-from-string (nth 2 (first eg-list))))))
        (cond
         ((and next-state-timestamp next-eg-timestamp (<= next-eg-timestamp next-state-timestamp))
          (setf timestamp next-eg-timestamp)
          (capi:apply-in-pane-process
           replay-pane
           #'(lambda () (gp:copy-pixels replay-pane buffer-pane
                                        redraw-x redraw-y redraw-width redraw-height redraw-x redraw-y)))

          (capi:apply-in-pane-process
           replay-pane
           #'(lambda ()
               (multiple-value-bind (x y w h) (draw-eg-event replay-pane (first eg-list))
                 (setf redraw-x x) (setf redraw-y y)
                 (setf redraw-width w) (setf redraw-height h))))
          (pop eg-list)
          (sleep (max 0.001 (/ (- (- timestamp data-start-time) (- (get-internal-real-time) start-time)) 1000)))
          )
         (next-state-timestamp
          (setf timestamp next-state-timestamp)
          (apply (replay-function (first (task-list *cw*))) (list buffer-pane (first state-list)))
          (capi:apply-in-pane-process
           replay-pane #'(lambda () (gp:copy-pixels replay-pane buffer-pane 0 0 1280 1024 0 0)))
          (pop state-list)
          (sleep (max 0.001 (/ (- (- timestamp data-start-time) (- (get-internal-real-time) start-time)) 1000)))))))

    (capi:display-message "Replay complete: ~a seconds." (/ (- (get-internal-real-time) start-time) 1000.0))
    (close replay-handle)
    (stop-experiment cw)
    )
  )

(defmethod stop-experiment ((cw cogworld))
  (with-slots (control-window startup-process status task-list listener-window) cw
    (log-info (list "CW-EVENT" "EXPERIMENT HALTED"))
    (if (capi:button-selected (delayed-file-io control-window)) (write-cw-log))
    (close-logging-file)
    (if startup-process  (mp:process-kill startup-process))
    (setf status :halted)
    (kill-monitor)                       ;; Kill the hot-regions event monitor
    (disconnect-eyetracker cw)
    (if (not (debug-p)) (hide-background-window))
    ;(show-menu-bar)
    (dolist (task task-list)      ;; Close each task in the task-list
      (if (break-function task) (apply (break-function task) nil)))

    (capi:execute-with-interface              ;; Show the CW control window
          control-window
          'capi:show-interface control-window)
    (if listener-window
      (capi:execute-with-interface              ;; Show the CW listener window
         listener-window
         'capi:show-interface listener-window))
    (capi:apply-in-pane-process               ;; Set the CW start/stop button to Start
         (button-start control-window)
         #'(lambda () (setf (capi:item-text (button-start control-window)) "Start")))
    (if *matlab-engine*
      (stop-matlab))     ;; Close MATLAB if its 
    ;(mp:process-run-function "done" '() (lambda () (sleep 1) (show-menu-bar)))
    (setf task-list nil)))

(defun stop-experiment-log-only ()
  (log-info (list "CW-EVENT" "EXPERIMENT HALTED"))
  (close-logging-file)
  (setf (status *cw*) :halted)
                         
  (disconnect-eyetracker *cw*)
  (show-menu-bar)
  (quit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cogworld (&key recompile)
  (when (and *cw* (control-window *cw*))
    (capi:destroy (control-window *cw*)))
  (when recompile
    (asdf:load-system 'cogworld))
  (setf *screen-width* (capi:screen-width (capi:convert-to-screen)))
  (setf *screen-height* (capi:screen-height (capi:convert-to-screen)))
  (setf *cw* (make-instance 'cogworld))
  (when (probe-file (merge-pathnames "defaults.lisp" *default-experiment-settings-file-directory*))
    (load (merge-pathnames "defaults.lisp" *default-experiment-settings-file-directory*)))
  (setf (json-rpc-server-process *cw*) (json-rpc-server-start))
  #+MACOSX
  (setf (local-path *cw*) (probe-file (format nil "/Applications/MultiWorld ~a/Data" *version-string*)))
  #+WIN32
  (setf (local-path *cw*) nil)

  ;; Kill the LispWorks toolbar window
#|
  (if (not *delivered*)
      (dolist (interface (capi:screen-interfaces (capi:convert-to-screen)))
        (if (equal (type-of interface)
                   'LISPWORKS-TOOLS::LISPWORKS-TOOLBAR-WINDOW)
            (capi:destroy interface))))
|#

  (if (not (probe-file *default-experiment-settings-file-directory*))
      (sys:make-directory *default-experiment-settings-file-directory*))

  (create-cw-windows)
  (define-settings)
  T)

(defun create-cw-windows ()
  (setf (control-window *cw*) (make-instance 'control-window))
  
  #|
  (let ((sound-path (format nil "~a" (merge-pathnames 
                                      (make-pathname
                                       :directory '(:RELATIVE "Source" "sounds")
                                       :name "resolve-reverse"
                                       :type "wav")
                                      (local-path *cw*)))))
    ;(play-sound-file sound-path)
    )
|#
  (capi:display (control-window *cw*))
  
  #+MACOSX(splash 1)
  )

;; Cleanup on exit
(defun shutdown-world (&rest args)
  (declare (ignore args))
  (kill-monitor)
  (json-rpc-server-stop)
  (setf *cw* nil)
  (setf *mw* nil)
  (when *delivered*
    (quit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun define-defaults (&key default-eyetracker-ip default-eeg-ip)
  (setf (default-eyetracker-ip *cw*) default-eyetracker-ip)
  (setf (default-eeg-ip *cw*) default-eeg-ip))

(defun define-settings
       (&key
        (experiment-name "")
        (experiment-version 1)
        (eyetracking nil)
        (eyetracker-ip (default-eyetracker-ip *cw*))
        (logging t)
        (pad nil)
        (eeg nil)
        (eeg-ip (default-eeg-ip *cw*))
        (log-dir "")
        (append-day-hr-min nil)
        (file-io nil)
        (sym->str nil)
        (file-list nil)
        (debug nil)
        (pi-email "grayw@rpi.edu")
        (matlab-dir #+MAC"/Applications/MATLAB_R2010a.app" #-MAC"")
        (python-bin #+MAC"/Library/Frameworks/Python.framework/Versions/Current/bin/python" #-MAC"/usr/bin/python")
        (conditions "")
        ;; Deprecated
        (color-vision-test nil)
        (log-fn "")
        (model-file nil)
        )

  (let ((interface (control-window *cw*)))

    (capi:apply-in-pane-process
     (conditions interface)
     (lambda () (setf (capi:text-input-pane-text (conditions interface)) conditions)))
    (capi:apply-in-pane-process
     (pi-email interface)
     #'(lambda () (setf (capi:text-input-pane-text (pi-email interface))
                        pi-email)))
    (capi:apply-in-pane-process
     (experiment-name interface)
     #'(lambda () (setf (capi:text-input-pane-text (experiment-name interface))
                        experiment-name)))
    (capi:apply-in-pane-process
     (experiment-version interface)
     #'(lambda () (setf (capi:text-input-pane-text (experiment-version interface))
                        (format nil "~a" experiment-version))))
    (capi:apply-in-pane-process
     (check-response-pad interface)
     #'(lambda () (setf (capi:button-selected (check-response-pad interface))
                        pad)))
    (capi:apply-in-pane-process
     (check-eeg interface)
     #'(lambda () (setf (capi:button-selected (check-eeg interface))
                        eeg)))
    (capi:apply-in-pane-process
     (eeg-ip interface)
     #'(lambda () (setf (capi:text-input-pane-text (eeg-ip interface))
                        eeg-ip)))
    (capi:apply-in-pane-process
     (check-eyetracker interface)
     #'(lambda () (setf (capi:button-selected (check-eyetracker interface))
                        eyetracking)))
    (capi:apply-in-pane-process
     (eyetracker-ip interface)
     #'(lambda () (setf (capi:text-input-pane-text (eyetracker-ip interface))
                        eyetracker-ip)))
    (capi:apply-in-pane-process
     (check-logging interface)
     #'(lambda () (setf (capi:button-selected (check-logging interface))
                        logging))) 
    (capi:apply-in-pane-process
     (logging-folder interface)
       (lambda () (setf (capi:text-input-pane-text (logging-folder interface)) log-dir)))
    (capi:apply-in-pane-process
     (delayed-file-io interface)
     #'(lambda () (setf (capi:button-selected (delayed-file-io interface))
                        file-io )))
    (capi:apply-in-pane-process
     (write-symbols-as-strings interface)
     #'(lambda () (setf (capi:button-selected (write-symbols-as-strings interface))
                        sym->str)))
    (capi:apply-in-pane-process
     (task-list interface)
     #'(lambda () (setf (capi:collection-items (task-list interface))
                        file-list)))
    (capi:apply-in-pane-process
     (delayed-file-io interface)
     #'(lambda () (setf (capi:button-selected (check-debug interface))
                        debug )))
    (capi:apply-in-pane-process
     (matlab-folder interface)
       (lambda () (setf (capi:text-input-pane-text (matlab-folder interface)) matlab-dir)))
    (capi:apply-in-pane-process
     (python-binary interface)
       (lambda () (setf (capi:text-input-pane-text (python-binary interface)) python-bin)))
    t))

(defun save-settings ()
  (if (null *experiment-settings-file*)
      nil
    (let* ((handle (open-file *experiment-settings-file*))
           (interface (control-window *cw*))
           (file-list nil))
      (dotimes (i (length (capi:collection-items (task-list interface))))
        (push (format nil "~s" (aref (capi:collection-items (task-list interface)) i))
              file-list))
      (setf file-list (reverse file-list))
      (write (read-from-string (format nil
"(define-settings
   :experiment-name \"~a\"
   :experiment-version ~a
   :eyetracking ~a
   :eyetracker-ip ~S
   :logging ~a
   :pad ~a
   :eeg ~a
   :eeg-ip ~S
   :log-dir ~S
   :file-io ~a
   :sym->str ~a
   :file-list \'~a
   :debug ~a
   :matlab-dir ~S
   :python-bin ~S
   :pi-email ~S
   :conditions ~S)"
(capi:text-input-pane-text (experiment-name interface))
(capi:text-input-pane-text (experiment-version interface))
(capi:button-selected (check-eyetracker interface))
(capi:text-input-pane-text (eyetracker-ip interface))
(capi:button-selected (check-logging interface)) 
(capi:button-selected (check-response-pad interface))
(capi:button-selected (check-eeg interface))
(capi:text-input-pane-text (eeg-ip interface))
(capi:text-input-pane-text (logging-folder interface))
(capi:button-selected (delayed-file-io interface))
(capi:button-selected (write-symbols-as-strings interface))
file-list
(capi:button-selected (check-debug interface))
(capi:text-input-pane-text (matlab-folder interface))
(capi:text-input-pane-text (python-binary interface))
(capi:text-input-pane-text (pi-email interface))
(capi:text-input-pane-text (conditions interface))
))
             :stream handle)
    (close handle)
    t)))

(defun debug-p ()
  (capi:button-selected (check-debug (control-window *cw*))))

(defmacro cw-debug-mode ()
  '(debug-p))

(defun eeg-p ()
  (capi:button-selected (check-eeg (control-window *cw*))))

(defun eeg-proc (func &rest args)
  (when (eeg-p) 
    (apply (read-from-string (concatenate 'string "eeg:" (symbol-name func))) args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility/external functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-rin ()
  (if (and *cw* (subject-info *cw*))
      (rin (subject-info *cw*))))

(defun get-uid ()
  (if (and *cw* (subject-info *cw*))
      (uid (subject-info *cw*))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun start-matlab ()
  (let ((dir (capi:text-input-pane-text (matlab-folder (control-window *cw*)))))
    (when (probe-file dir)
      (when (matlab:init dir)
       (setf *matlab-engine* (matlab:eng-open "matlab -maci"))))))

(defun run-matlab-task (path)
  (let ((cmd (concatenate 'string "cd('" path "'); Cogworld('Connect'); Cogworld('Socket'); Cogworld('Disconnect');")))
    (matlab:eng-eval-string *matlab-engine* cmd)))

(defun stop-matlab ()
  (matlab:eng-close *matlab-engine*))