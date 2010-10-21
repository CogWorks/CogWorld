(defparameter *version-string* "2.0")

(defparameter *default-experiment-settings-file-directory* (concatenate 'string (format nil "~a" (sys:get-folder-path :documents)) "CogWorld/"))

(defparameter *mw* nil) ;; Provided for backward compatability: use *cw*
(defparameter *cw* nil)
(defparameter *delivered*
  #+:delivered t
  #-:delivered nil)

(defparameter *use-matlab* 0)
(defparameter *matlab-engine* nil)

(defparameter *experiment-settings-file* nil)

(defparameter *task-conditions* nil)

(defparameter *color-test* nil)

(let ((base (directory-namestring (asdf:system-definition-pathname 'cogworld))))
  ;; Image files
  (defparameter *splash* (gp:read-external-image (merge-pathnames "images/new-splash.bmp" base)))
  (defparameter *rpi* (gp:read-external-image (merge-pathnames "images/RPI.bmp" base)))
  (defparameter *cogworks* (gp:read-external-image (merge-pathnames "images/CogWorks.bmp" base)))
  (defparameter *sponsors* (gp:read-external-image (merge-pathnames "images/Sponsors.bmp" base)))
  ;; Icon files
  (defparameter *list-add* (gp:read-external-image (merge-pathnames "images/icons/edit_add.png" base)))
  (defparameter *list-remove* (gp:read-external-image (merge-pathnames "images/icons/edit_remove.png" base)))
  (defparameter *go-up* (gp:read-external-image (merge-pathnames "images/icons/1downarrow.png" base)))
  (defparameter *go-down* (gp:read-external-image (merge-pathnames "images/icons/1uparrow.png" base))))

(defparameter *screen-width* nil)
(defparameter *screen-height* nil)