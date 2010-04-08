;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename     : control-win.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; Version      : 2.0.0
;; Description  : Defines the MultiWorld control window
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; History
;;
;; [2005.03.10] : File created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(capi:define-interface control-window ()
  ((visible :initform t :accessor visible)
   (pending-debug-messages :initform nil :accessor pending-debug-messages)
   )

  (:panes
   (experiment-name
    capi:text-input-pane
    :accessor experiment-name
    :title "Name:"
    :title-position :top
    :visible-min-width '(character 25)
    :visible-max-width '(character 25))
   (experiment-version
    capi:text-input-pane
    :accessor experiment-version
    :title "Version:"
    :title-position :top
    :visible-min-width '(character 5)
    :visible-max-width '(character 5))
;;;;;;; Model ;;;;;;;
   (model-file
    capi:text-input-pane
    :visible-min-width '(character 25)
    :visible-max-width '(character 25)
    :accessor model-file)
   (title-model
    capi:title-pane
    :title "Model location:")
   (button-model
    capi:push-button
    :callback 'button-model-push
    :enabled t
    :text "...")
;;;;;;; Human ;;;;;;;;
   (check-logging
    capi:check-button
    :title "Options:"
    :title-position :top
    :text "Data logging"
    :accessor check-logging)
   (check-debug
    capi:check-button
    :text "Debug mode"
    :accessor check-debug)
   (check-eyetracker
    capi:check-button
    :text "Eyetracker"
    :selected nil
    :enabled
    #+MACOSX t
    #-MACOSX nil
    :accessor check-eyetracker)

   (check-response-pad
    capi:check-button
    :text "Response Pad"
    :selected nil
    :enabled
    #+MACOSX t
    #-MACOSX nil
    :accessor check-response-pad)

   (check-eeg
    capi:check-button
    :text "EEG"
    :selected nil
    :enabled
    #+MACOSX t
    #-MACOSX nil
    :accessor check-eeg)

   (color-vision
    capi:check-button
    :text "Color Vision Test"
    :selected nil
    :enabled t
    :accessor color-vision)
   
   (actr-environment
    capi:check-button
    :text "Environment"
    :selected nil
    :enabled t
    :accessor actr-environment)


   (task-list
    capi:list-panel
    :internal-border 5
    :interaction :multiple-selection 
    :visible-min-width '(character 31)
    :visible-max-width '(character 31)
    :visible-min-height '(character 8)
    :visible-max-height '(character 8)
    :print-function #'(lambda (item)
                        (format nil "~a.~a"
                                (pathname-name item)
                                (pathname-type item)))
    :horizontal-scroll
    #+MACOSX nil
    #-MACOSX t
    :accessor task-list)
   (button-add-task
    capi:push-button
    :callback 'button-add-push
    :text "Add...")
   (button-remove-task
    capi:push-button
    :callback 'button-remove-push
    :text "Remove")
   
;;;;
   (button-start
    capi:push-button
    :callback 'button-start-push
    :default-p t
    :enabled t
    :visible-min-width 130
    :accessor button-start
    :text "Start")
   (button-save-settings
    capi:push-button
    :callback 'button-save-settings-push
    :visible-min-width 130
    :text "Save Settings")
;;;;
   (logging-folder capi:title-pane :title "Logging Folder: " :text " " :accessor logging-folder :visible-min-width 300)
   (choose-logging-folder capi:push-button :title "Choose Logging Folder" :text "..." :accessor choose-logging-folder 
                          :selection-callback 'logging-folder-callback)
   (logging-fn capi:text-input-pane :title "Log File Name: " :text " " :accessor logging-fn :visible-max-width 100 :visible-min-width 100)
   (fn-date capi:check-button :text "Append day-hr-min" :selected nil :accessor fn-date)
   (delayed-file-io capi:check-button :text "Write file when quit" :selected nil :accessor delayed-file-io)
   (write-symbols-as-strings capi:check-button :text "Write symbols as strings" :selected nil :accessor write-symbols-as-strings)
;;;;
   )
  (:layouts
   (row-0 capi:row-layout '(experiment-name experiment-version ))
   (exp-info  capi:column-layout '(row-0 check-debug) :title "Experiment Info:" :title-position :frame)
   (log-layout capi:column-layout '( logging-folder choose-logging-folder logging-fn fn-date delayed-file-io write-symbols-as-strings) 
               :gap 20)
   (file-buttons capi:row-layout '(button-add-task button-remove-task))
   (tasks capi:column-layout '(task-list file-buttons) :title "Tasks:" :title-position :frame :adjust :left)
   (equip-row capi:row-layout '( check-eyetracker check-response-pad check-eeg))
   (equip-row1 capi:row-layout '( check-eyetracker  check-eeg))
   (human-layout capi:column-layout '(check-logging equip-row color-vision tasks))
   (model-layout capi:row-layout '(title-model model-file button-model)) 
   (actr-layout capi:column-layout '(model-layout actr-environment tasks))
   (replay-layout capi:column-layout '())
   (remote-layout capi:column-layout '(check-logging equip-row1 color-vision tasks ))
   (control-layout capi:tab-layout '() :items '(("Human" human-layout)
                                              ("ACT-R" actr-layout)
                                              ("Logging" log-layout)
                                              ("Remote" remote-layout))
                 :visible-min-width 340 :visible-max-width 440
                 :print-function 'first :visible-child-function 'second :accessor control-layout)
   (buttons capi:column-layout '(button-start button-save-settings ))
   (main capi:column-layout '(exp-info   control-layout buttons )  :adjust :center :accessor main :gap 50) ;options tasks buttons)
   )
  (:default-initargs
   :title (format nil "CogWorld v~a" *version-string*)
   :destroy-callback 'shutdown-world
   :initial-focus 'experiment-name
   :visible-min-height 700
   :visible-min-width 380
   :layout 'main
   :x 30
   :y 30
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun button-add-push (&rest args)
  (declare (ignore args))
  (let ((cur-vector (capi:collection-items (task-list (control-window *mw*))))
        (cur-list nil)
        (choice (capi:prompt-for-file "Select a file:"
                                 :filter "*.*")))
    (if choice
        (progn  (dotimes (i (length cur-vector))
                  (push (aref cur-vector i) cur-list))
          (setf (capi:collection-items (task-list (control-window *mw*)))
                (cons choice cur-list))
          ))))

(defun button-remove-push (&rest args)
  (declare (ignore args))
  (let ((choices (capi:choice-selected-items (task-list (control-window *mw*))))
        (cur-vector (capi:collection-items (task-list (control-window *mw*))))
        (cur-list nil))
    (when choices
      (dotimes (i (length cur-vector))
        (push (aref cur-vector i) cur-list))
      (dolist (choice choices)
        (setf (capi:collection-items (task-list (control-window *mw*)))
              (remove choice cur-list))))))

(defun button-model-push (&rest args)
  (declare (ignore args))
  (let ((choice (capi:prompt-for-file "Select a file:"
                                 :filter "*.*")))
    (if choice
        (setf (capi:text-input-pane-text (model-file (control-window *mw*)))
              (format nil "~a" choice)))
    )
  )

(defun button-save-settings-push (&rest args)
  (declare (ignore args))
  (save-settings)
  (capi:display-message "Settings saved.")
  )

(defun button-start-push (data interface)
  (declare (ignore data))
  (cond
   ((equal (capi:item-text (button-start interface )) "Start")
    (if (capi:button-selected (check-debug interface)) (cw-debug-mode t))
    (setf (experiment-version *cw*) (read-from-string (capi:text-input-pane-text (experiment-version interface))))
    (setf (capi:item-text (button-start interface )) "Stop")
    (mp:process-run-function
     "Start experiment"
     nil 'start-experiment *cw*))
   (t
    (setf (capi:item-text (button-start interface)) "Start")
    (mp:process-run-function
     "Stop experiment"
     nil 'stop-experiment *cw*)))
  )

(defun logging-folder-callback (data interface)
  (declare (ignore data))
  (let ((pn (capi:prompt-for-directory "Locate Directory")))
    (when pn
      (capi:apply-in-pane-process (logging-folder interface) 
                                       #'(setf capi:title-pane-text)  (directory-namestring pn) (logging-folder interface)))))
      

#|
(defun process-control-mode (data interface)
  (capi:display-message (format nil "~S ~S" data interface))
  (cond ((equal data "Eye Data Log")
         (when (eye-only interface)
           (capi:apply-in-pane-process (logging-folder interface) 
                                       #'(setf capi:title-pane-text)  (directory-namestring (eye-only interface)) (logging-folder interface))
           (capi:apply-in-pane-process (logging-fn interface) 
                                       #'(setf capi:text-input-pane-text)  (file-namestring (eye-only interface)) (logging-fn interface)))
         (capi:apply-in-pane-process (main interface) 
                 (lambda (pane) (if (null (member 'log-layout (capi:layout-description pane)))
                                    (setf (capi:layout-description pane)
                                     (append (capi:layout-description pane) '(log-layout)))))
                 (main interface)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hide-control-window ()
  (if (and *mw* (control-window *mw*))
      (progn (setf (visible (control-window *mw*)) nil)
        (capi:execute-with-interface
         (control-window *mw*)
         #'(lambda ()                      
             (capi:hide-interface (task-list (control-window *mw*)) nil))))
    )
  )

(defun show-control-window ()
  (if (and *mw* (control-window *mw*))
      (progn (setf (visible (control-window *mw*)) t)
        (capi:execute-with-interface
         (control-window *mw*)
         #'(lambda ()                      
             (capi:show-interface (task-list (control-window *mw*)))))
        )
    )
  )

(defun control-window-visible ()
  (visible (control-window *mw*))
  )
