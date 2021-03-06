;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename     : listener-win.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author       : Chris R. Sims
;; Copyright    : (C) 2005 Chris R. Sims
;; Address      : Cognitive Science Department
;;              : Rennselaer Polytechnic Institute
;;              : Troy, NY 12180
;;              : simsc@rpi.edu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Version      : 2.0.0
;; Description  : Defines the MultiWorld listener window
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

(capi:define-interface listener-window ()
  ((visible :initform t :accessor visible))
  (:panes
   (listener
    capi:listener-pane
    :font (gp:make-font-description :family "Monaco" :size 12)
    ;:background (color:make-rgb 0.9 0.9 0.9)
    ;:foreground (color:make-rgb 0.0 1.0 0.0)
    :accessor listener
    :visible-min-width 600
    :visible-min-height 300)
   )
  (:menus
   (edit-menu "Edit" (("Cut"
                       :callback 'edit-cut
                       :callback-type :interface)
                      ("Copy"
                       :callback 'edit-copy
                       :callback-type :interface)
                      ("Paste"
                       :callback 'edit-paste
                       :callback-type :interface))))
  (:menu-bar edit-menu)
  (:layouts
   (main capi:column-layout '(listener))
   )
  (:default-initargs
   :title (format nil "MultiWorld v~a - Listener Window" *version-string*)
   :x 330
   :y 30)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Callbacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun edit-cut (interface)
  #+MACOSX(call-editor (listener interface) "Kill Region")
  )

(defun edit-copy (interface)
  #+MACOSX(call-editor (listener interface) "Save Region")
  )

(defun edit-paste (interface)
  #+MACOSX(call-editor (listener interface) "Un-Kill")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun hide-listener-window ()
  (if (and *cw* (listener-window *cw*))
      (progn (setf (visible (listener-window *cw*)) nil)
        (capi:apply-in-pane-process
         (listener (listener-window *cw*))
         #'(lambda ()                      
             (capi:hide-interface (listener (listener-window *cw*)) nil))))
    )
  )

(defun show-listener-window ()
  (if (and *cw* (listener-window *cw*))
      (progn (setf (visible (listener-window *cw*)) t)
        (capi:apply-in-pane-process
         (listener (listener-window *cw*))
         #'(lambda ()                      
             (capi:show-interface (listener (listener-window *cw*)))))
        )
    )
  )

(defun listener-window-visible ()
  (visible (listener-window *cw*))
  )