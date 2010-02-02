;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename     : toolbar.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Author       : Mike Schoelles
;; Copyright    : (C) 2005, 2009 CogWorks Laboratory
;; Address      : Cognitive Science Department
;;              : Rennselaer Polytechnic Institute
;;              : Troy, NY 12180
;;              : schoem@rpi.edu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(capi:define-interface cw-toolbar ()
    ()
  (:panes
    (toolbar1 capi:toolbar :items (mapcar (lambda (n x txt) (make-instance 'capi:toolbar-button :help-key txt :image n :callback x)) 
                             '(:std-file-open :std-file-save :std-find  ) '(start-eyetracking-callback stop-eyetracking-callback stop-experiment-log-only-callback) '("Start Eyetracking" "Stop Eyetracking" "Quit")) 
                           :accessor toolbar1))
                           
   (:layouts
     (docking capi:docking-layout '() :items '(toolbar1) :accessor docking))
   (:default-initargs
     :title "Multiworld"
     :help-callback 'helpme
     :layout 'docking
     :best-x 0 
     :best-y 0))

(defun helpme (interface pane type help-key)
  (declare (ignore interface pane type))
  ;(capi:display-message "~S" help-key)
  (cond ((stringp help-key)
         help-key)))

(defun start-eyetracking-callback (data interface)
  (declare (ignore data interface))
  (start-eyetracking))

(defun stop-eyetracking-callback (data interface)
  (declare (ignore data interface))
  (stop-eyetracking))

(defun stop-experiment-log-only-callback  (data interface)
  (declare (ignore data interface))
   (if (capi:button-selected (delayed-file-io (control-window *cw*)))
       (write-cw-log))
  (stop-experiment-log-only))