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
    :visible-min-width '(character 25))
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
    :visible-min-width '(character 40)
    :visible-min-height '(character 8)
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
    :width 10
    :callback 'button-add-push
    :text "Add")
   (button-remove-task
    capi:push-button
    :width 10
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
   (exp-info capi:row-layout '(experiment-name experiment-version) :ratios '(1 1) :title "Experiment Info:" :title-position :frame)
   (log-folder-info capi:column-layout '(logging-folder choose-logging-folder logging-fn))
   (log-opts capi:column-layout '(fn-date delayed-file-io write-symbols-as-strings))
   (log-layout capi:row-layout '(log-folder-info log-opts) :title "Logging Options:" :title-position :frame :adjust :left)
   (file-buttons capi:column-layout '(nil button-add-task button-remove-task nil) :ratios '(1 1) :adjust :center)
   (tasks capi:row-layout'(task-list file-buttons)
          :title "Tasks:"
          :title-position :frame
          :adjust :left
          :ratios '(1 1))
   (options capi:column-layout '(check-debug check-logging check-eyetracker check-response-pad check-eeg color-vision)
            :title "Options:"
            :title-position :frame
            :adjust :left)
   (buttons capi:column-layout '(button-start))
   (exp-opts capi:row-layout '(options tasks) :ratios '(1 1))
   (main capi:column-layout '(exp-info exp-opts log-layout buttons ) :adjust :center :accessor main)
   )
  (:default-initargs
   :title (format nil "CogWorld v~a" *version-string*)
   :destroy-callback 'shutdown-world
   :initial-focus 'experiment-name
   :layout 'main
   :toolbar-items (list 
                   (make-instance
                    'capi:toolbar-component
                    :items
                    (list
                     (make-instance 'capi:toolbar-button
                                    :text "New"
                                    :image :std-file-new
                                    :callback 'button-new-push)
                     (make-instance 'capi:toolbar-button
                                    :text "Open"
                                    :image :std-file-open
                                    :callback 'button-open-push)
                     (make-instance 'capi:toolbar-button
                                    :text "Save"
                                    :image :std-file-save
                                    :callback 'button-save-settings-push))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Event Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun button-add-push (&rest args)
  (declare (ignore args))
  (let ( (choice (capi:prompt-for-file "Select a file:"
                                 :filter "*.*")))
    (aif choice (capi:append-items (task-list (control-window *mw*)) (list it)))))
        

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

(defun button-new-push (data interface)
  (declare (ignore data)
           (ignore interface))
  (define-settings))

(defun button-open-push (data interface)
  (declare (ignore data)
           (ignore interface))
  (let ((path (capi:prompt-for-file
               nil
               :pathname *default-experiment-settings-file-directory*
               :operation :open)))
    (if (not (null path))
        (load path))))

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
