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
;; Description  : Defines the MultiWorld splash (startup) window
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; History
;;
;; [2005.03.10] : File created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(capi:define-interface splash-win ()
  ((cogworks-logo :initform 'img-splash
                  :accessor cogworks-logo))
  (:panes
   (cogworks-pane capi:output-pane
                  :accessor cogworks-pane
                  :display-callback 'display-splash-images
                  :visible-min-width 500
                  :visible-max-width 322
                  :visible-min-height 322))
  (:layouts
   (main capi:simple-layout '(cogworks-pane)))
  (:default-initargs
   :x (- (/ (capi:screen-width (capi:convert-to-screen)) 2)
         (/ 500 2))
   :y (- (/ (capi:screen-height (capi:convert-to-screen)) 2)
         (/ 322 2))
   :window-styles '(:always-on-top :borderless :internal-borderless))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Callbacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-splash-images (pane &rest args)
  (declare (ignore args))
  (let* ((interface (capi:top-level-interface pane))
         (cogworks-pane (cogworks-pane interface))
         (cogworks-image (gp:load-image cogworks-pane
                                        (cogworks-logo interface))))
    
    (gp:draw-image cogworks-pane cogworks-image 0 0)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun splash (splash-time)
  (let ((interface (make-instance 'splash-win)))
    (capi:display interface)
    (sleep splash-time)
    (capi:execute-with-interface interface 'capi:destroy interface)))