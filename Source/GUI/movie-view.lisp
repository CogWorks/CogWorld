
(capi:define-interface movie-view ()
  ((movie-path :initform nil :initarg :movie-path :accessor movie-path))
  (:panes
   (movie-pane
    capi:cocoa-view-pane
    :view-class "NSMovieView" :accessor movie-pane)
   (info-pane
    capi:title-pane :accessor info-pane)
   (controls
    capi:push-button-panel
    :items '( "Play" "Done")
    :callbacks '(movie-play movie-done)
    :callback-type :interface)
   )
  (:layouts
   (main-layout
    capi:column-layout
    '(movie-pane info-pane controls)))
  (:default-initargs
   :layout 'main-layout
   :title "Movie View"
   :best-width 700
   :best-height 550))

(defmethod movie-done ((self movie-view))
  (capi:destroy self))

(defmethod movie-play ((self movie-view))
  (with-slots (movie-pane info-pane movie-path) self
    (let ((url (objc:invoke "NSURL"
                              "fileURLWithPath:"
                              (namestring movie-path))))

      (objc:invoke (capi:cocoa-view-pane-view movie-pane)
                     "setMovie:"
                     (objc:invoke (objc:invoke "NSMovie" "alloc")
                                  "initWithURL:byReference:"
                                  url
                                  nil)))
    (let ((view (capi:cocoa-view-pane-view movie-pane)))
      (objc:invoke view "gotoBeginning:" view)
      (objc:invoke view "start:" view))))

#|
(defun movie-view-test-open (self)
  (with-slots (movie-pane info-pane) self
    (when-let (path (capi:prompt-for-file "Movie file"
                                          :filters '("Movie files" "*.mov")))
      (setf (capi:title-pane-text info-pane)
            (format nil "Loaded movie ~A" path))
      (let ((url (objc:invoke "NSURL"
                              "fileURLWithPath:"
                              (namestring path))))

        (objc:invoke (capi:cocoa-view-pane-view movie-pane)
                     "setMovie:"
                     (objc:invoke (objc:invoke "NSMovie" "alloc")
                                  "initWithURL:byReference:"
                                  url
                                  nil))))))



(defmethod open-movie ((self movie-view) path)
  (with-slots (movie-pane info-pane dir-path movie-path) self
    (setf movie-path path )
    (setf (capi:title-pane-text info-pane)
            (format nil "Movie ~A" (file-namestring movie-path)))
    (let ((url (objc:invoke "NSURL"
                              "fileURLWithPath:"
                              (namestring movie-path))))
        (objc:invoke (capi:cocoa-view-pane-view movie-pane)
                     "setMovie:"
                     (objc:invoke (objc:invoke "NSMovie" "alloc")
                                  "initWithURL:byReference:"
                                  url
                                  nil)))))

(defun mm (self)
  (with-slots (movie-pane info-pane) self
    (when-let (path (capi:prompt-for-file "Movie file"
                                          :filters '("Movie files" "*.mov")))
      (setf (capi:title-pane-text info-pane)
            (format nil "Loaded movie ~A" path))
      (let ((url (objc:invoke "NSURL"
                              "fileURLWithPath:"
                              (namestring path))))

        (objc:invoke (capi:cocoa-view-pane-view movie-pane)
                     "setMovie:"
                     (objc:invoke (objc:invoke "NSMovie" "alloc")
                                  "initWithURL:byReference:"
                                  url
                                  nil))))))

|#