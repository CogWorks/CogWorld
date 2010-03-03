(defun done-dialog (data win)
  (declare (ignore data win))
  (capi:abort-dialog))
          
(capi:define-interface edinburgh ()
  ()
  (:panes
   (description capi:display-pane
                :text "Which hand do you prefer to use when:"
                :background :transparent
                :font (gp:make-font-description
                       :family :stock
                       :weight :bold))
   (row1 capi:radio-button-panel
         :title "Writing:"
         :title-gap 30
         :title-position :left
         :items (likert-button-list "Left" "" "Right")
         :layout-args '(:gap 10))
   (row2 capi:radio-button-panel
          :title "Drawing:"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10))
   (row3 capi:radio-button-panel
          :title "Throwing:"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10))
   (row4 capi:radio-button-panel
          :title "Using Scissors:"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10))
   (row5 capi:radio-button-panel
          :title "Using a Toothbrush:"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10))
   (row6 capi:radio-button-panel
          :title "Using a Knife (without a fork):"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10))
   (row7 capi:radio-button-panel
          :title "Using a Spoon:"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10))
   (row8 capi:radio-button-panel
          :title "Using a broom (upper hand):"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10))
   (row9 capi:radio-button-panel
          :title "Striking a Match:"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10))
   (row10 capi:radio-button-panel
          :title "Opening a Box (holding the lid):"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10))
   (button-done capi:push-button
                :text "Done"
                :callback 'done-dialog
                :default-p t))
  (:layouts
   (main capi:column-layout '(description inventory buttons) :adjust :left)
   (inventory capi:column-layout '(row1 row2 row3 row4 row5 row6 row7 row8 row9 row10) :adjust :right :accessor inventory)
   (buttons capi:row-layout '(nil button-done nil) :adjust :center)
   )
  (:default-initargs
   :title "The Edinburgh Handedness Inventory")
  )
     
(defmethod initialize-instance :after ((win edinburgh) &rest args)
  (let ((panels (capi:layout-description (inventory win))))
    (dolist (panel panels)
      (setf (capi:choice-selection (slot-value win panel)) nil))))

(capi:display-dialog (make-instance 'edinburgh))