(pushnew :cogworld *features*)

(defparameter *version-string* "2.0")

#-LINUX
(defparameter *default-experiment-settings-file-directory* (concatenate 'string (format nil "~a" (sys:get-folder-path :documents)) "CogWorld/"))
#+LINUX
(defparameter *default-experiment-settings-file-directory* "~/.config/CogWorld")

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
  (defparameter *list-add* (gp:read-external-image (merge-pathnames "images/icons/add.png" base)))
  (defparameter *list-remove* (gp:read-external-image (merge-pathnames "images/icons/remove.png" base)))
  (defparameter *go-down* (gp:read-external-image (merge-pathnames "images/icons/go-down.png" base)))
  (defparameter *go-up* (gp:read-external-image (merge-pathnames "images/icons/go-up.png" base)))
  (defparameter *filenew* (gp:read-external-image (merge-pathnames "images/icons/filenew.png" base)))
  (defparameter *fileopen* (gp:read-external-image (merge-pathnames "images/icons/fileopen.png" base)))
  (defparameter *filesave* (gp:read-external-image (merge-pathnames "images/icons/filesave.png" base)))
  (defparameter *filesaveas* (gp:read-external-image (merge-pathnames "images/icons/filesaveas.png" base)))
  (defparameter *email* (gp:read-external-image (merge-pathnames "images/icons/mail-forward.png" base))))

(defparameter *screen-width* nil)
(defparameter *screen-height* nil)
