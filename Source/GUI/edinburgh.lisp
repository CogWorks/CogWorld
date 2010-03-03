(defmacro likert-button-left (label)
  `(make-instance 'capi:radio-button
                  :text ""
                  :title ,label
                  :title-position :left))

(defmacro likert-button-right (label)
  `(make-instance 'capi:radio-button
                  :text ""
                  :title ,label
                  :title-position :right
                  :title-gap -10))

(defmacro likert-button-center ()
  `(make-instance 'capi:radio-button
                  :text ""))

(defmacro likert-button-list (&rest labels)
  `(let ((len (list-length ',labels))
         (likert-buttons '()))
     (let ((mid (/ len 2.0)))
       (let ((sidelen (floor mid)))
         (loop for i from 0 to (1- sidelen) do
               (push (likert-button-right (nth i (reverse ',labels))) likert-buttons))
         (cond
          ((/= mid sidelen)
           (push (likert-button-center) likert-buttons)
           (loop for i from (1+ sidelen) to (1- len) do
                 (push (likert-button-left (nth i (reverse ',labels))) likert-buttons)))
          (t (loop for i from sidelen to (1- len) do
                 (push (likert-button-left (nth i (reverse ',labels))) likert-buttons)))
          )))
     likert-buttons))
          
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
   )
  (:layouts
   (main capi:column-layout '(description inventory) :adjust :left)
   (inventory capi:column-layout '(row1 row2 row3 row4 row5 row6 row7 row8 row9 row10) :adjust :right)
   )
  (:default-initargs
   :title "The Edinburgh Handedness Inventory")
  )
