;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename     : event.lisp
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
;; Description  : Handles system event monitoring
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; History
;;
;; [2005.04.19] : File created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse monitor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mouse-monitor (event-type)
  (if (equal event-type :click)
      (log-info (append (list "MW-EVENT" "MOUSE-CLICK") (mouse-position)))))

(defun monitor-region (x1 y1 x2 y2 callback &key (callback-type :string))
  (push (make-instance 'hot-region
                       :x1 x1
                       :y1 y1
                       :x2 x2
                       :y2 y2
                       :callback callback
                       :callback-type callback-type
                       :entered nil)
        (hot-regions *cw*)))

(defun monitor-regions ()
  (setf (monitor-process *cw*)
        (mp:process-run-function
         "Monitor regions"
         nil
         'monitor-regions-internal)))

(defun kill-monitor ()
  (if (and (monitor-process *cw*)
           (mp:process-p (monitor-process *cw*)))
      (progn (mp:process-kill (monitor-process *cw*))
        (setf (hot-regions *cw*) nil))))

(defun monitor-regions-internal ()
  (let ((pos nil)
        (mouse-x nil)
        (mouse-y nil)
        (change-state nil)
        (button-down nil)
        (button-state nil))
    (loop
     (when (not (eq (status *cw*) :running)) (return))
     (capi:process-pending-messages nil)
     (sleep 0.02)
     (setf button-state (get-button-state))
     (if (equal button-down button-state)
         (setf change-state nil)
       (setf change-state t))
     (setf button-down button-state)

     (setf pos (mouse-position))
     (setf mouse-x (first pos))
     (setf mouse-y (second pos))
     
     (dolist (region (hot-regions *cw*))
       (cond
        ((and
          (not (entered region))
          (> mouse-x (x1 region))
          (< mouse-x (x2 region))
          (> mouse-y (y1 region))
          (< mouse-y (y2 region))
          (callback region))
         (mp:process-run-function
          "Enter callback" nil
          'apply (callback region) (list (if (equal (callback-type region) :keyword)
                                             :enter
                                           "ENTER")))
         (setf (entered region) t))
        ((and
          (entered region)
          (or (> mouse-x (x2 region))
              (< mouse-x (x1 region))
              (> mouse-y (y2 region))
              (< mouse-y (y1 region)))
          (callback region))
         (mp:process-run-function
          "Leave callback" nil
          'apply (callback region) (list (if (equal (callback-type region) :keyword)
                                             :leave
                                           "LEAVE")))
         (setf (entered region) nil))
        ((and
          (entered region)
          change-state
          (= button-state 1)
          (callback region))
         (mp:process-run-function
          "Click callback" nil
          'apply (callback region) (list (if (equal (callback-type region) :keyword)
                                             :click
                                           "CLICK")))
         ))))
    ))
