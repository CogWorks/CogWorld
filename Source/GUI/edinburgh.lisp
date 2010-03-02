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
          :title-position :left
          :items '("Left" "No Pref" "Right"))
   (row2 capi:radio-button-panel
          :title "Drawing:"
          :title-position :left
          :items '("Left" "No Pref" "Right"))
   (row3 capi:radio-button-panel
          :title "Throwing:"
          :title-position :left
          :items '("Left" "No Pref" "Right"))
   (row4 capi:radio-button-panel
          :title "Using Scissors:"
          :title-position :left
          :items '("Left" "No Pref" "Right"))
   (row5 capi:radio-button-panel
          :title "Using a Toothbrush:"
          :title-position :left
          :items '("Left" "No Pref" "Right"))
   (row6 capi:radio-button-panel
          :title "Using a Knife (without a fork):"
          :title-position :left
          :items '("Left" "No Pref" "Right"))
   (row7 capi:radio-button-panel
          :title "Using a Spoon:"
          :title-position :left
          :items '("Left" "No Pref" "Right"))
   (row8 capi:radio-button-panel
          :title "Using a broom (upper hand):"
          :title-position :left
          :items '("Left" "No Pref" "Right"))
   (row9 capi:radio-button-panel
          :title "Striking a Match:"
          :title-position :left
          :items '("Left" "No Pref" "Right"))
   (row10 capi:radio-button-panel
          :title "Opening a Box (holding the lid):"
          :title-position :left
          :items '("Left" "No Pref" "Right"))
   )
  (:layouts
   (main capi:column-layout '(description inventory) :adjust :left)
   (inventory capi:column-layout '(row1 row2 row3 row4 row5 row6 row7 row8 row9 row10) :adjust :right)
   )
  (:default-initargs
   :title "The Edinburgh Handedness Inventory")
  )
