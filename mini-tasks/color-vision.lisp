;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename     : ichihar-color-vision.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Author       : Mike Schoelles, Mike Bruzinski
;; Copyright    : (C) 2009 CogWorks Laboratory
;; Address      : Cognitive Science Department
;;              : Rennselaer Polytechnic Institute
;;              : Troy, NY 12180
;;              : schoem@rpi.edu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run (do-color-test)

(defun color-test () 
  (color-task *cw*))

(defmethod initialize-instance :after ((obj color-task) &rest args)
  (with-slots (color-test-images color-test-list) obj
    (dolist (test-pair color-test-list)
      (push (gp:read-external-image (concatenate 'string 
                                    (directory-namestring (cw-path *cw*))  "Images/colortest-images/" "cimage" (princ-to-string (first test-pair)) ".jpg")) 
            color-test-images))))

;;----------------------------------------------------------------------------
;; logging
;;----------------------------------------------------------------------------

(defun log-color-test-header ()
          (log-info ; cw logging
           (list
            "CW-INFO" ; info-type
            "test-detail" ; info-detail 
            "id" ; trial-trial-block-id
            "test-value" ; subj-trial-id 
            "response" ; exp-trial-id 
            )))

(defun log-color-test-details (id test-value response)
          (log-info ; cw logging
           (list
            "CW-INFO" ; info-type
            "test-detail" ; info-detail 
            id ; trial-trial-block-id
            test-value ; subj-trial-id 
            response ; exp-trial-id 
            )))


;;----------------------------------------------------------------------------
;; images
;;----------------------------------------------------------------------------

(defmethod get-color-vision-test-objs ((p color-task))
  (with-slots (center-x center-y) p
    (let ((test-image-list nil))
      (dolist (vt-external-image (color-test-images p))
        ;; create the pinboard object
        (push (make-instance 'capi:image-pinboard-object :image vt-external-image :x (- center-x (floor 500 2)) :y (- center-y (floor 489 2)))
              test-image-list)) 
      test-image-list ) ))

;;----------------------------------------------------------------------------
;; interface
;;----------------------------------------------------------------------------

(capi:define-interface interface-color-test () 
  ((color-test-p :initform nil :accessor color-test-p)
   (current-color-test :initform nil :accessor current-color-test)
   (subj-response :initform nil :accessor subj-response)) 
  (:panes
   (color-test-input capi:text-input-pane
                     :accessor color-test-input
                     :title "Enter number or ?: "
                     :visible-max-width 30 :visible-min-width 30
                     :callback 'vision-test-callback)
   (results-txt1 capi:item-pinboard-object
                   :text "You have completed the color vision test."
                   :x (- (task-center-x (color-test)) 125) 
                   :y (task-center-y (color-test)))
   (results capi:item-pinboard-object :text ""
                   :x (- (task-center-x (color-test)) 125) 
                   :y (+ 40 (task-center-y (color-test)))
                   :accessor results)
   (results-rg capi:item-pinboard-object :text ""
                   :x (- (task-center-x (color-test)) 125) 
                   :y (+ 80 (task-center-y (color-test)))
                   :accessor results-rg)
   (results-txt2 capi:item-pinboard-object
                   :text "Press Space Bar to Continue"
                   :x (- (task-center-x (color-test)) 125) 
                   :y (+ 120 (task-center-y (color-test))))
   ) 
  (:layouts 
   (color-test-image capi:pinboard-layout () :accessor color-test-image)
   (input-row capi:row-layout '(nil color-test-input nil))
   (color-test-layout capi:column-layout
                      '(color-test-image input-row)
                      :accessor color-test-layout
                     ; :background :black
                     ; :foreground :white
                      :font (gp:make-font-description 
                             :family "times" 
                             :size 18 
                             :weight :medium                         
                             :slant :roman))
   (start-layout capi:pinboard-layout
                 (list
                  (make-instance
                   'capi:item-pinboard-object
                   :text "Press space bar to start."
                   :x (- (task-center-x (color-test)) 200) 
                   :y (task-center-y (color-test)))) 
                 :accessor start-layout
                 :background :black
                 :foreground :white
                 :font (gp:make-font-description 
                        :family "times" 
                        :size 18 
                        :weight :medium                         
                        :slant :roman)
                 :input-model `((:character start-space-callback )))
   (end-layout capi:pinboard-layout
               '(results-txt1 results results-rg results-txt2) 
               :background :black
               :foreground :white
               :font (gp:make-font-description 
                      :family "times" 
                      :size 18 
                      :weight :medium                         
                      :slant :roman)
               :input-model `((:character start-space-callback))
               :accessor end-layout)
   (task-switchable-layout
    capi:switchable-layout
    '(start-layout end-layout color-test-layout)
    :accessor task-switchable-layout
    :visible-child 'start-layout
    :title-position :frame
    :visible-max-width nil
    :visible-max-height nil)
   )

  (:default-initargs
   :title (task-title (color-test))
   :best-width *screen-width* 
   :best-height *screen-height* 
   :layout 'task-switchable-layout
   )
)
;;----------------------------------------------------------------------------
;; callbacks
;;----------------------------------------------------------------------------

(defun vision-test-callback (data interface)
 (with-slots (current-color-test color-test-p color-test-input) interface
   (log-color-test-details (first current-color-test) (second current-color-test) data)
   (let ((val (ignore-errors (parse-integer data))))
     (cond ((or val (equal data "?"))
            (setf color-test-p t) ; flag to move to next test image
            (push (list (second current-color-test)  (third current-color-test) (read-from-string data)) (subj-response interface)))
           (t
            (capi:display-message "Response must be an integer or ?")))
     (capi:apply-in-pane-process color-test-input
                             (lambda () 
                               (setf (capi:text-input-pane-text color-test-input) ""))))))

(defun start-space-callback (self x y character)
  (declare (ignore x y)) 
  (if (eq character '#\space) (setf (color-test-p (capi:element-interface self)) t)))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod switch-trial-layout ((layout-in capi:simple-pane))
  "Trial layout within investigator"
  (setf (capi:switchable-layout-visible-child (task-switchable-layout (color-test-window (color-test))))
        layout-in))


(defmethod switch-screen-layout ((layout-in capi:simple-pane))
  "Screen within experiment"
  (setf (capi:switchable-layout-visible-child (task-switchable-layout (color-test-window (color-test)) ))
        layout-in))

(defmethod show-end ((win interface-color-test))
  (with-slots (task-switchable-layout end-layout subj-response results results-rg ) win
    ;; switch layout to end-layout
    (let ((normal% (if subj-response (floor (* 100 (/ (count-if (lambda(x) (eql (first x) (third x))) subj-response) (length subj-response)) )) 0))
          (red/green% (if subj-response (floor (* 100 (/ (count-if (lambda(x) (eql (second x) (third x))) subj-response) (length subj-response)) )) 0)))
      (setf (capi:item-text results) (format nil "You got ~S% for normal vision." normal%))
      (setf (capi:item-text results-rg) (format nil "You got ~S% for red/green color blind." red/green%))
      (capi:apply-in-pane-process task-switchable-layout 'switch-screen-layout end-layout)
      (capi:apply-in-pane-process task-switchable-layout 'capi:set-pane-focus end-layout)
    (list normal% red/green%))))
  
(defmethod set-color-vision-test (ext-img (win interface-color-test))
  (with-slots (color-test-image) win
    (capi:apply-in-pane-process color-test-image 
                                'capi:manipulate-pinboard color-test-image ext-img :add-top) ))

(defmethod show-color-vision-test ((win interface-color-test))
  (with-slots (task-switchable-layout color-test-layout color-test-input) win
 ;; switch layout to color-test-layout
  (capi:apply-in-pane-process task-switchable-layout 'switch-trial-layout color-test-layout)
 ;; set focus
  (capi:apply-in-pane-process color-test-input 'capi:set-pane-focus color-test-input)))

;;----------------------------------------------------------------------------
;; experiment
;;----------------------------------------------------------------------------

(defun do-color-test ()
  (setf (color-task *cw*) (make-instance 'color-task))
  (let* ((p (color-test))
         (win (make-instance 'interface-color-test))
         (objs (get-color-vision-test-objs p))
         (results nil))
    (setf (color-test-window p) win)
    (capi:display win)
    (mp:process-wait "Waiting for blank" 'color-test-p win)
    (setf (color-test-p win) nil)
    (log-color-test-header)
    (dotimes (i (length objs))
      (setf (current-color-test win) (nth i (color-test-list p)))
      ;; color vision test
      (set-color-vision-test (nth i objs) win)
      (show-color-vision-test win)
      (setf (color-test-p win) nil)
      ;; wait for subject to press start button
      (mp:process-wait "Waiting for subject to color vision test results" 'color-test-p win))  
    (setq results (show-end win))
    (setf (color-test-p win) nil)
    (mp:process-wait "Waiting for blank" 'color-test-p win)
    (capi:apply-in-pane-process win 'capi:destroy win)
    results))
