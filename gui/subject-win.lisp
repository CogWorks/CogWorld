;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename     : subject-win.lisp
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
;; Description  : Defines the MultiWorld subject registration window
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; History
;;
;; [2005.03.10] : File created
;; [2009.08.24] : New Privacy constraints for IRB, added experiment history
;; [2009.08.24] : Converted to Cogworld
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(capi:define-interface subject-window ()
  ()
  (:panes
   (first-name
    capi:text-input-pane
    :title-position :top
    :visible-max-width '(character 16)
    :visible-min-width '(character 16)
    :accessor text-first-name)
   (last-name
    capi:text-input-pane
    :title-position :top
    :visible-max-width '(character 16)
    :visible-min-width '(character 16)
    :accessor text-last-name)
   (rin
    capi:text-input-pane
    :title-position :top
    :visible-max-width '(character 11)
    :visible-min-width '(character 11)
    :max-characters 9
    :accessor text-rin)
   (age
    capi:text-input-pane
    :title-position :top
    :visible-max-width '(character 10)
    :visible-min-width '(character 10)
    :accessor text-age)
   (gender
    capi:text-input-pane
    :title-position :top
    :visible-max-width '(character 10)
    :visible-min-width '(character 10)
    :accessor text-gender)
   (major
    capi:text-input-pane
    :title-position :top
    :visible-max-width '(character 16)
    :visible-min-width '(character 16)
    :accessor text-major)
   (exps capi:popup-menu-button 
         :text "Previous Experiments" 
         :accessor exps
         :menu (make-instance 'capi:menu  
                              :items (list (make-instance 'capi:menu-component 
                                     :items '(
                                              "Vogel" "Argus" "NavBack" "RVS" "Obvis" "SpaceFortress" "DMAP" "Cluster"
                                                  ) 
                                     :interaction :multiple-selection))))
   (num-blocks
    capi:text-input-pane
    :title "Num-blocks:"
    :title-position :top
    :visible-max-width '(character 10)
    :visible-min-width '(character 10)
    :accessor num-blocks)
   (num-trials
    capi:text-input-pane
    :title "Num-trials:"
    :title-position :top
    :visible-max-width '(character 10)
    :visible-min-width '(character 10)
    :accessor num-trials)
   (button-done
    capi:push-button
    :text "Done"
    :callback 'registration-done
    :default-p t)
   )
  (:layouts
   (main capi:column-layout '(info-col exps button-done) :adjust :center)
   (info-col capi:grid-layout '(
                                "First name:" first-name
                                "Last name:" last-name
                                "RIN:" rin
                                "Age:" age
                                "Gender:" gender
                                "Major:" major
                                ) :accessor info-col)
   (debug-row capi:row-layout '(num-blocks num-trials) :accessor debug-row)
   )
  (:default-initargs
   :title "Participant Info"
   :x (- (/ (capi:screen-width (capi:convert-to-screen)) 2) 100)
   :y (- (/ (capi:screen-height (capi:convert-to-screen)) 2) 100))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Callbacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-uid ()
  (multiple-value-bind (second minute hour day month year daylight zone other) (get-decoded-time)
    (declare (ignore  daylight zone other))
      (format nil "~2,'0D~2,'0D~2,'0D_~2,'0D~2,'0D~2,'0D"
                         (mod year 2000)
                         month
                         day
                         hour
                         minute
                         second)))

(defun registration-done (data win)
  (declare (ignore data))
  (let* ((info (make-instance 'subject-info-class))
         (exp-menu (first (capi:menu-items (capi:popup-menu-button-menu (exps win)))))
         (sel-idxs (capi:choice-selection exp-menu))
         (items (capi:collection-items exp-menu))
         (rin-text (capi:text-input-pane-text (text-rin win))))
    (cond ((not (eql 9 (length rin-text)))
           (cond ((null (cw-debug-mode))
                  (capi:display-message "RIN must be 9 characters in length, reenter")
                  (return-from registration-done))
                 (t
                  (setf rin-text (subseq (concatenate 'string rin-text "000000000") 0 9))))))
    (setf (first-name info) (capi:text-input-pane-text (text-first-name win)))
    (setf (last-name info) (capi:text-input-pane-text (text-last-name win)))
    (setf (major info) (capi:text-input-pane-text (text-major win)))
    (setf (gender info) (capi:text-input-pane-text (text-gender win)))
    (setf (rin info) rin-text)
    (setf (age info) (capi:text-input-pane-text (text-age win)))
    (setf (uid info) (make-uid))
    (dolist (idx sel-idxs)
      (push  (capi:item-data (elt items idx)) (exp-history info)))
    (let ((blks (ignore-errors (parse-integer (capi:text-input-pane-text (num-blocks win)))))
          (trs (ignore-errors (parse-integer (capi:text-input-pane-text (num-trials win))))))
      (if blks (setf (num-blocks info) blks))
      (if trs (setf (num-trials info) trs))) 
    (setf (subject-info *cw*) info)
    
  (capi:execute-with-interface
   win
   #'(lambda () (capi:destroy win)))))

(defmethod register-subject ((p cogworld))
  (setf (subject-window p) (make-instance 'subject-window))
  (case (cw-debug-mode)
    (all
     (setf (subject-info p) (make-instance 'subject-info-class
                 :first-name "debug" :last-name "debug" :rin "123456789") )
     (return-from register-subject))
     
    (num-trials
     (setf (capi:layout-description (info-col (subject-window p))) 
                            (append  (capi:layout-description (info-col (subject-window p))) 
                                     (list (debug-row (subject-window p)))))))
  (capi:display (subject-window p)) 
  (setf (startup-process p) mp:*current-process*) 
  (mp:process-wait
   "Waiting for subject registration."
   #'(lambda ()
       (subject-info p)))
  (setf (startup-process p) nil)
  )