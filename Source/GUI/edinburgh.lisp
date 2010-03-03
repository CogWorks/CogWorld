(defun done-dialog (data win)
  (declare (ignore data win))
  (capi:abort-dialog))

(defun iscomplete (data win)
  (let ((missing '())
        (panels (capi:layout-description (inventory win))))
    (dolist (panel (rest panels))
      (let ((p (slot-value win panel)))
        (if (= 0 (list-length (capi:choice-selected-items p)))
            (push "!" missing))))
    (if (zerop (list-length missing))
        (setf (capi:button-enabled (done win)) t))))

(capi:define-interface edinburgh ()
  ()
  (:panes
   (title capi:display-pane
                :text "Handedness Questionnaire"
                :background :transparent
                :font (gp:make-font-description
                       :family :stock
                       :size 20
                       :weight :bold))
   (instructions capi:display-pane
                :text "Instructions"
                :background :transparent
                :font (gp:make-font-description
                       :family :stock
                       :size 16
                       :weight :bold))
   (inst capi:display-pane
                :text "For each of the activities below, please indicate:"
                :background :transparent
                :font (gp:make-font-description
                       :family :stock
                       :size 15
                       :weight :medium))
   (inst1 capi:display-pane
                :text "Which hand you prefer for that activity?"
                :background :transparent
                :font (gp:make-font-description
                       :family :stock
                       :size 12
                       :weight :light
                       :slant :italic))
   (inst2 capi:display-pane
                :text "Do you ever use the other hand for the activity?"
                :background :transparent
                :font (gp:make-font-description
                       :family :stock
                       :size 12
                       :weight :light
                       :slant :italic))
   (row1 capi:radio-button-panel
         :title "Writing:"
         :title-gap 30
         :title-position :left
         :items (likert-button-list "Left" "no pref" "Right")
         :layout-args '(:gap 10)
         :selection-callback 'iscomplete)
   (row2 capi:radio-button-panel
          :title "Drawing:"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10)
          :selection-callback 'iscomplete)
   (row3 capi:radio-button-panel
          :title "Throwing:"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10)
          :selection-callback 'iscomplete)
   (row4 capi:radio-button-panel
          :title "Using Scissors:"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10)
          :selection-callback 'iscomplete)
   (row5 capi:radio-button-panel
          :title "Using a Toothbrush:"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10)
          :selection-callback 'iscomplete)
   (row6 capi:radio-button-panel
          :title "Using a Knife (without a fork):"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10)
          :selection-callback 'iscomplete)
   (row7 capi:radio-button-panel
          :title "Using a Spoon:"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10)
          :selection-callback 'iscomplete)
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
          :layout-args '(:gap 10)
          :selection-callback 'iscomplete)
   (row10 capi:radio-button-panel
          :title "Opening a Box (holding the lid):"
          :title-gap 30
          :title-position :left
          :items (likert-button-list "Left" "" "Right")
          :layout-args '(:gap 10)
          :selection-callback 'iscomplete)
   (button-done capi:push-button
                :text "Evaluate"
                :callback 'done-dialog
                :default-p nil
                :accepts-focus-p nil
                :enabled nil
                :accessor done)
   (np0 capi:display-pane
         :text "                 "
         :background :transparent)
   (np1 capi:display-pane
         :text ""
         :background :transparent)
   (np2 capi:display-pane
         :text "no pref"
         :background :transparent)
   (oh1 capi:display-pane
         :text "Do you ever use"
         :background :gray75)
   (oh2 capi:display-pane
         :text "the other hand?"
         :background :gray75)
   (h1 capi:check-button :text "Yes    " :data "y1")
   (h2 capi:check-button :text "Yes    " :data "y2")
   (h3 capi:check-button :text "Yes    " :data "y3")
   (h4 capi:check-button :text "Yes    " :data "y4")
   (h5 capi:check-button :text "Yes    " :data "y5")
   (h6 capi:check-button :text "Yes    " :data "y6")
   (h7 capi:check-button :text "Yes    " :data "y7")
   (h8 capi:check-button :text "Yes    " :data "y8")
   (h9 capi:check-button :text "Yes    " :data "y9")
   (h10 capi:check-button :text "Yes    " :data "y10"))
  (:layouts
   (main capi:column-layout '(ta da ia ba) :adjust :left :gap 10)
   (ta capi:row-layout '(nil title nil))
   (da capi:column-layout '(instructions inst i0) :gap 15)
   (i0 capi:column-layout '(i1 i2))
   (i1 capi:row-layout '(nil inst1 nil) :adjust :center)
   (i2 capi:row-layout '(nil inst2 nil) :adjust :center)
   (ia capi:row-layout '(inventory otherhands) :title "Which hand do you prefer to use when:" :title-position :frame)
   (inventory capi:column-layout '(row0 row1 row2 row3 row4 row5 row6 row7 row8 row9 row10) :adjust :right :accessor inventory)
   (row0 capi:row-layout '(col0 np0))
   (col0 capi:column-layout '(np1 np2))
   (otherhands capi:column-layout '(h0 h1 h2 h3 h4 h5 h6 h7 h8 h9 h10) :adjust :right)
   (h0 capi:column-layout '(oh1 oh2) :background :gray75)
   (ba capi:row-layout '(nil button-done nil) :adjust :center)
   )
  (:default-initargs
   :title "The Edinburgh Handedness Inventory")
  )
     
(defmethod initialize-instance :after ((win edinburgh) &rest args)
  (let ((panels (capi:layout-description (inventory win))))
    (dolist (panel (rest panels))
      (let ((p (slot-value win panel)))
        (setf (capi:choice-selection p) nil)
        (setf (capi:simple-pane-background p) :gray85)))))
;        (let ((buttons (capi:collection-items p)))
 ;         (loop for button across buttons do
  ;              (setf (capi:simple-pane-foreground button) :red)))))))

(capi:display-dialog (make-instance 'edinburgh))
