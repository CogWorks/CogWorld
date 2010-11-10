;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename     : background.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author       : Mike Schoelles, Chris R. Sims
;; Copyright    : (C) 2005, 2009 CogWorks Laboratory
;; Address      : Cognitive Science Department
;;              : Rennselaer Polytechnic Institute
;;              : Troy, NY 12180
;;              : schoem@rpi.edu
;;

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Version      : 2.0.0
;; Description  : Defines the MultiWorld background window
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

(capi:define-interface background-win ()
  ((visible :initform t :accessor visible))
  (:panes
   (draw-pane capi:output-pane
              :visible-min-width (capi:screen-width (capi:convert-to-screen))
              :visible-min-height (capi:screen-height (capi:convert-to-screen))
              :input-model '(((:button-1 :press :shift) click-halt)
                             ((:button-1 :press) handle-bkgd-win-click)
                             )
              :display-callback 'redraw-background-window
              :accepts-focus-p nil
              :font (gp:make-font-description :family "Courier" :size 18)
              :background (color:make-rgb 0.2 0.2 0.6 1.0)
              :accessor draw-pane
              )
   )
  (:layouts
   (main capi:simple-layout '(draw-pane))
   )
  (:default-initargs
   :x 0
   :y 0
   :window-styles '(:borderless :internal-borderless)
   :background (color:make-rgb 0.2 0.2 0.6 1.0)
   :title "CogWorks Lab"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Callbacks & event handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun redraw-background-window (pane &optional &rest args)
  (declare (ignore args))
  (if (and (eyetracker *cw*)
           (draw-commands (eyetracker *cw*)))
      (progn
        (dolist (command 
                 (subseq
                  (draw-commands (eyetracker *cw*))
                  0 (min (length (draw-commands (eyetracker *cw*))) 50)))
          (case (car command)
            (:eg-draw-cross
             (let ((x (first (cdr command)))
                   (y (second (cdr command)))
                   (diam (third (cdr command)))
                   (color (fourth (cdr command))))
               (gp:draw-lines
                pane
                (list (- x diam) y (+ x diam) y
                      x (- y diam) x (+ y diam))
                :foreground color)))
            (:eg-draw-circle
             (let ((x (first (cdr command)))
                   (y (second (cdr command)))
                   (diam (third (cdr command)))
                   (color (fourth (cdr command))))
               (gp:draw-circle
                pane x y diam :foreground color :filled t)
               (gp:draw-circle
                pane x y 1 :foreground :black :filled t)
               ))
            (:eg-display-text
             (let ((text (first (cdr command)))
                   (x (second (cdr command)))
                   (y (third (cdr command)))
                   
                   (color (fifth (cdr command))))
               (gp:draw-string
                pane text x y :foreground color
                :font (capi:simple-pane-font pane)))))))))

(defun click-halt (self x y)
  (declare (ignore self x y))
  (stop-experiment *cw*))

(defmethod handle-bkgd-win-click ((self capi:output-pane) x y)
  (declare (ignore self x y))
  (dolist (interface (capi:screen-interfaces (capi:convert-to-screen)))
    (if (slot-exists-p interface 'parse)
        (capi:execute-with-interface
         interface
         'capi:raise-interface
         interface))))


 
(defun raise-tasks (&optional self x y)
  (declare (ignore self x y))
  (dolist (interface (capi:screen-interfaces (capi:convert-to-screen)))
    (if (slot-exists-p interface 'parse)
        (capi:execute-with-interface
         interface
         'capi:raise-interface
         interface))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-background-window ()
  (if (and *cw* (background-window *cw*) (not (visible (background-window *cw*))))
      (progn
        (kiosk-mode t)
        (setf (visible (background-window *cw*)) t)
        (capi:execute-with-interface
         (background-window *cw*)
         #'(lambda ()                      
             (capi:show-interface  (background-window *cw*)))))))

(defun hide-background-window ()
  (if (and *cw* (background-window *cw*))
      (progn 
        (kiosk-mode nil)
        (setf (visible (background-window *cw*)) nil)
        (capi:execute-with-interface
         (background-window *cw*)
         #'(lambda ()                      
             (capi:hide-interface (background-window *cw*) nil))))))


(defun create-background-window ()
  
  (if (background-window *cw*)
      (capi:execute-with-interface
       (background-window *cw*)
       #'(lambda ()
           (capi:destroy (background-window *cw*)))))
  (setf (background-window *cw*) (make-instance 'background-win))
  (kiosk-mode t)
  (capi:execute-with-interface
   (background-window *cw*)
   #'(lambda ()
       (progn
         (capi:display (background-window *cw*))
         (capi:set-top-level-interface-geometry (background-window *cw*)
                                           :x 0
                                           :y 0)))))
