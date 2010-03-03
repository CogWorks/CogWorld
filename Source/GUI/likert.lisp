(defmacro likert-button-left (label)
  `(make-instance 'capi:radio-button
                  :data ,label
                  :text ""
                  :title ,label
                  :title-position :left
                  :accepts-focus-p nil))

(defmacro likert-button-right (label)
  `(make-instance 'capi:radio-button
                  :data ,label
                  :text ""
                  :title ,label
                  :title-position :right
                  :title-gap -10
                  :accepts-focus-p nil))

(defmacro likert-button-center (label)
  `(make-instance 'capi:radio-button
                  :data ,label
                  :text ""
                  :accepts-focus-p nil))

(defmacro likert-button-list (&rest labels)
  `(let ((len (list-length ',labels))
         (likert-buttons '()))
     (let ((mid (/ len 2.0)))
       (let ((sidelen (floor mid)))
         (loop for i from 0 to (1- sidelen) do
               (push (likert-button-right (nth i (reverse ',labels))) likert-buttons))
         (cond
          ((/= mid sidelen)
           (push (likert-button-center (nth sidelen (reverse ',labels))) likert-buttons)
           (loop for i from (1+ sidelen) to (1- len) do
                 (push (likert-button-left (nth i (reverse ',labels))) likert-buttons)))
          (t (loop for i from sidelen to (1- len) do
                 (push (likert-button-left (nth i (reverse ',labels))) likert-buttons)))
          )))
     likert-buttons))
