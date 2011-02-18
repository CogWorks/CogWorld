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
   ;;;;;;; Info ;;;;;;;
   (exp-name
    capi:text-input-pane
    :accessor experiment-name
    :title "Name:"
    :visible-min-width '(character 32))
   (exp-ver
    capi:text-input-pane
    :accessor experiment-version
    :title "Version:"
    :visible-min-width '(character 5)
    :visible-max-width '(character 5))
   (pi-email
    capi:text-input-pane
    :accessor pi-email
    :title "Primary Investigator Email:"
    :visible-min-width '(character 32))
   ;;;;;;; Tasks ;;;;;;;;
   (task-list
    capi:list-panel
    :internal-border 5
    :interaction :single-selection
    :accepts-focus-p nil
    :visible-min-width '(character 20)
    :print-function #'(lambda (item)
                        (format nil "~a.~a"
                                (pathname-name item)
                                (pathname-type item)))
    :accessor task-list)
   (button-add-task
    capi:push-button
    :callback 'button-add-push
    :image *list-add*)
   (button-remove-task
    capi:push-button
    :callback 'button-remove-push
    :image *list-remove*)
   (button-up-task
    capi:push-button
    :callback 'button-up-push
    :image *go-up*)
   (button-down-task
    capi:push-button
    :callback 'button-down-push
    :image *go-down*)
   ;;;;;;; Debug ;;;;;;;;
   (check-debug
    capi:check-button
    :text "Enable Debugging"
    :accessor check-debug)
   ;;;;;;; MATLAB ;;;;;;;;
   (matlab-folder
    capi:text-input-pane
    :title "MATLAB Folder:"
    :accessor matlab-folder
    :visible-min-width '(character 30))
    (choose-matlab-folder
     capi:push-button
     :text "..."
     :accessor choose-matlab-folder 
     :selection-callback 'matlab-folder-callback)
   ;;;;;;; PYTHON ;;;;;;;;
   (python-binary
    capi:text-input-pane
    :title "Python Binary:"
    :accessor python-binary
    :visible-min-width '(character 30))
    (choose-python-binary
     capi:push-button
     :text "..."
     :accessor choose-python-binary 
     :selection-callback 'python-file-callback)
   ;;;;;;; Logging ;;;;;;;;
   (check-logging
    capi:check-button
    :text "Enable Logging"
    :accessor check-logging)
   (logging-folder
    capi:text-input-pane
    :title "Logging Folder:"
    :accessor logging-folder
    :visible-min-width '(character 30))
    (choose-logging-folder
     capi:push-button
     :text "..."
     :accessor choose-logging-folder 
     :selection-callback 'logging-folder-callback)
    (delayed-file-io
     capi:check-button
     :text "Write file when quit"
     :accessor delayed-file-io)
    (write-symbols-as-strings
     capi:check-button
     :text "Write symbols as strings"
     :accessor write-symbols-as-strings)
   ;;;;;;; Eye Tracker ;;;;;;;;
   (check-eyetracker
    capi:check-button
    :text "Enable Eye Tracker"
    :accessor check-eyetracker)
   (eyetracker-addy
    capi:text-input-pane
    :title "EGServer IP:"
    :accessor eyetracker-ip
    :visible-min-width '(character 16)
    :visible-max-width '(character 16)
    :max-characters 15)
   ;;;;;;;: Conditions ;;;;;;;;;
   (conditions
    capi:multi-line-text-input-pane
    :title "Comma separated list of task conditions:"
    :title-position :above
    :text ""
    :accessor conditions)
   ;;;;;;; Response Pad ;;;;;;;;
   (check-response-pad
    capi:check-button
    :text "Enable Response Pad"
    :accessor check-response-pad)
   ;;;;;;; EEG ;;;;;;;;
   (check-eeg
    capi:check-button
    :text "Enable EEG"
    :accessor check-eeg)
   (eeg-addy
    capi:text-input-pane
    :title "NetStation IP:"
    :accessor eeg-ip
    :visible-min-width '(character 16)
    :visible-max-width '(character 16)
    :max-characters 15)
   ;;;;;;; RUN ;;;;;;;;
   (button-start
    capi:push-button
    :callback 'button-start-push
    :default-p t
    :enabled t
    :visible-min-width 130
    :accessor button-start
    :text "Start")
   ;;;; OPTIONS ;;;;
   (options-list
    capi:list-panel
    :internal-border 5
    :interaction :single-selection
    :visible-min-width '(character 20)
    :print-function 'first
    :selection-callback 'update-switchable
    :accepts-focus-p nil
    )
   )
  (:layouts
   (exp-info capi:row-layout '(exp-name exp-ver))
   (log-folder-row capi:row-layout '(logging-folder choose-logging-folder))
   (log-info capi:column-layout '(check-logging delayed-file-io write-symbols-as-strings log-folder-row pi-email))
   (eeg-info capi:column-layout '(check-eeg eeg-addy))
   (eyetracker-info capi:column-layout '(check-eyetracker eyetracker-addy))
   (matlab-folder-row capi:row-layout '(matlab-folder choose-matlab-folder))
   (matlab-info capi:column-layout '(matlab-folder-row))
   (python-binary-row capi:row-layout '(python-binary choose-python-binary))
   (python-info capi:column-layout '(python-binary-row))
   (debug-info capi:column-layout '(check-debug))
   (task-buttons capi:column-layout '(nil button-up-task button-add-task button-remove-task button-down-task nil) :ratios '(1 nil nil nil nil 1) :adjust :center)
   (tasks capi:row-layout '(task-list task-buttons) :ratios '(1 nil))
   (condition-info capi:column-layout '(nil conditions nil))
   (buttons capi:row-layout '(nil button-start nil) :ratios '(1 nil 1))
   (switchable capi:switchable-layout '(tasks log-info eeg-info eyetracker-info check-response-pad matlab-info python-info debug-info) :visible-child 'tasks)
   (options capi:row-layout '(options-list switchable) :gap 10 :ratios '(nil 1) :title "Options:" :title-position :frame :adjust :left :internal-border 10)
   (main capi:column-layout '(exp-info options buttons) :ratios '(nil 1))
   )
  (:default-initargs
   :title (format nil "CogWorld v~a" *version-string*)
   :destroy-callback 'shutdown-world
   ;:initial-focus 'exp-name-field
   :layout 'main
   :visible-min-width 700
   :visible-min-height 320
   :toolbar-items (list 
                   (make-instance
                    'capi:toolbar-component
                    :items
                    (list
                     (make-instance 'capi:toolbar-button
                                    :text "New"
                                    :image *filenew*
                                    :callback 'button-new-push)
                     (make-instance 'capi:toolbar-button
                                    :text "Open"
                                    :image *fileopen*
                                    :callback 'button-open-push)
                     (make-instance 'capi:toolbar-button
                                    :text "Save"
                                    :image *filesave*
                                    :callback 'button-save-settings-push)
                     (make-instance 'capi:toolbar-button
                                    :text "Save As"
                                    :image *filesaveas*
                                    :callback 'button-saveas-settings-push)
                     (make-instance 'capi:toolbar-component)
                     (make-instance 'capi:toolbar-button
                                    :text "Save IDs"
                                    :image *email*
                                    :callback 'button-email-push)
                     (make-instance 'capi:toolbar-component)
                     )))))

(defmethod initialize-instance :after ((win control-window) &key)
  (with-slots (options-list tasks condition-info log-info eeg-info eyetracker-info check-response-pad matlab-info python-info debug-info) win
      (setf (capi:collection-items options-list)
            (list
             (list "Tasks" tasks)
             (list "Task Conditons" condition-info)
             (list "Logging" log-info)
             (list "Eye Tracker" eyetracker-info)
             (list "EEG" eeg-info)
             (list "Response Pad" check-response-pad)
             (list "MATLAB" matlab-info)
             (list "Python" python-info)
             (list "Debugging" debug-info)
             )))
  (if (not *delivered*)
      (dolist (interface (capi:screen-interfaces (capi:convert-to-screen)))
        (if (equal (type-of interface)
                   'LISPWORKS-TOOLS::LISPWORKS-TOOLBAR-WINDOW)
            (capi:destroy interface)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-switchable (data interface)
  (with-slots (switchable) interface
    (setf (capi:switchable-layout-visible-child switchable) (second data))))

(defun button-add-push (&rest args)
  (declare (ignore args))
  (let ( (choice (capi:prompt-for-file "Select a file:"
                                 :filter "*.*"
                                 :file-package-is-directory t)))
    (aif choice (capi:append-items (task-list (control-window *cw*)) (list it)))))
        
(defun button-remove-push (&rest args)
  (declare (ignore args))
  (let ((selected (capi:choice-selection (task-list (control-window *cw*))))
        (tlist (capi:collection-items (task-list (control-window *cw*)))))
    (when selected
      (setf (capi:collection-items (task-list (control-window *cw*))) (delete-nth tlist selected)))))

(defun button-up-push (&rest args)
  (declare (ignore args))
  (let ((selected (capi:choice-selection (task-list (control-window *cw*))))
        (tlist (capi:collection-items (task-list (control-window *cw*)))))
    (when (and selected (> (length tlist) 1) (> selected 0))
      (rotatef (elt tlist selected) (elt tlist (1- selected)))
      (setf (capi:collection-items (task-list (control-window *cw*))) tlist)
      (setf (capi:choice-selection (task-list (control-window *cw*))) (1- selected)))))

(defun button-down-push (&rest args)
  (declare (ignore args))
  (let ((selected (capi:choice-selection (task-list (control-window *cw*))))
        (tlist (capi:collection-items (task-list (control-window *cw*)))))
    (when (and selected (> (length tlist) 1) (< selected (- (length tlist) 1)))
      (rotatef (elt tlist selected) (elt tlist (1+ selected)))
      (setf (capi:collection-items (task-list (control-window *cw*))) tlist)
      (setf (capi:choice-selection (task-list (control-window *cw*))) (1+ selected)))))

(defun button-save-settings-push (data interface)
  (declare (ignore data)
           (ignore interface))
  (if (null *experiment-settings-file*)
      (setf *experiment-settings-file* (capi:prompt-for-file
                                        nil
                                        :pathname *default-experiment-settings-file-directory*
                                        :operation :save)))
  (if (not (null *experiment-settings-file*))
      (progn
        (save-settings)
        (capi:display-message "Settings saved."))))

(defun button-saveas-settings-push (data interface)
  (declare (ignore data)
           (ignore interface))
  (if (not (null *experiment-settings-file*))
      (setf *experiment-settings-file* (capi:prompt-for-file
                                        nil
                                        :pathname *experiment-settings-file*
                                        :operation :save))
    (setf *experiment-settings-file* (capi:prompt-for-file
                                        nil
                                        :pathname *default-experiment-settings-file-directory*
                                        :operation :save)))
  (if (not (null *experiment-settings-file*))
      (progn
        (save-settings)
        (capi:display-message "Settings saved."))))

(defun button-new-push (data interface)
  (declare (ignore data)
           (ignore interface))
  (define-settings))

(defun button-open-push (data interface)
  (declare (ignore data)
           (ignore interface))
  (setf *experiment-settings-file* (capi:prompt-for-file
                                    nil
                                    :pathname *default-experiment-settings-file-directory*
                                    :operation :open))
  (if (not (null *experiment-settings-file*))
      (load *experiment-settings-file*)))

(defun button-start-push (data interface)
  (declare (ignore data))
  (if (string-equal (capi:item-text (button-start interface )) "Start")
      (multiple-value-bind (value ret)
          (capi:popup-confirmer
           (make-instance 'capi:option-pane
                          :selection-callback :redisplay-interface
                          :items (append '("") (remove nil (split-sequence "," (capi:text-input-pane-text (conditions interface))))))
           "Choose a condition:"
           :value-function 'capi:choice-selected-item
           :ok-check #'(lambda (x) (not (string-equal x ""))))
        (when ret
          (progn
            (setf (task-condition *cw*) value)
            (setf (experiment-version *cw*) (read-from-string (capi:text-input-pane-text (experiment-version interface))))
            (setf (capi:item-text (button-start interface )) "Stop")
            (mp:process-run-function "Start experiment" nil 'start-experiment *cw*))))
    (progn
      (setf (capi:item-text (button-start interface)) "Start")
      (mp:process-run-function "Stop experiment" nil 'stop-experiment *cw*)))
  )

(defun button-email-push (data interface)
  (declare (ignore data))
  (cond ((equal "" (capi:text-input-pane-text  (experiment-name interface)))
         (capi:display-message "Please enter an experiment name"))
        (t
         (finish-experiment 
          (capi:text-input-pane-text (experiment-name interface))
          (capi:text-input-pane-text (pi-email interface))
          (capi:text-input-pane-text (logging-folder interface))))))

(defun logging-folder-callback (data interface)
  (declare (ignore data))
  (let ((pn (capi:prompt-for-directory "Locate Log Directory")))
    (when pn
      (capi:apply-in-pane-process (logging-folder interface) 
                                  #'(setf capi:text-input-pane-text) (directory-namestring pn) (logging-folder interface)))))

(defun matlab-folder-callback (data interface)
  (declare (ignore data))
  (let ((pn (capi:prompt-for-directory "Locate MATLAB Directory"
                                       :file-package-is-directory t)))
    (when pn
      (capi:apply-in-pane-process (matlab-folder interface) 
                                  #'(setf capi:text-input-pane-text) (directory-namestring pn) (matlab-folder interface)))))

(defun python-file-callback (data interface)
  (declare (ignore data))
  (let ((pn (capi:prompt-for-file "Locate Python Binary")))
    (when pn
      (capi:apply-in-pane-process (python-binary interface) 
                                  #'(setf capi:text-input-pane-text) (directory-namestring pn) (python-binary interface)))))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hide-control-window ()
  (if (and *cw* (control-window *cw*))
      (progn (setf (visible (control-window *cw*)) nil)
        (capi:execute-with-interface
         (control-window *cw*)
         #'(lambda ()                      
             (capi:hide-interface (task-list (control-window *cw*)) nil))))
    )
  )

(defun show-control-window ()
  (if (and *cw* (control-window *cw*))
      (progn (setf (visible (control-window *cw*)) t)
        (capi:execute-with-interface
         (control-window *cw*)
         #'(lambda ()                      
             (capi:show-interface (task-list (control-window *cw*)))))
        )
    )
  )

(defun control-window-visible ()
  (visible (control-window *cw*))
  )
