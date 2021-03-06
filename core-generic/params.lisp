(pushnew :cogworld *features*)

(defparameter *version-string* "2.1")

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
  (gp:register-image-translation 'img-splash (gp:read-external-image (merge-pathnames "images/new-splash.bmp" base)))
  (gp:register-image-translation 'img-rpi (gp:read-external-image (merge-pathnames "images/RPI.bmp" base)))
  (gp:register-image-translation 'img-cogworks (gp:read-external-image (merge-pathnames "images/CogWorks.bmp" base)))
  (gp:register-image-translation 'img-sponsors (gp:read-external-image (merge-pathnames "images/Sponsors.bmp" base)))
  ;; Icon files
  (gp:register-image-translation 'img-list-add (gp:read-external-image (merge-pathnames "images/icons/add.png" base)))
  (gp:register-image-translation 'img-list-remove (gp:read-external-image (merge-pathnames "images/icons/remove.png" base)))
  (gp:register-image-translation 'img-go-down (gp:read-external-image (merge-pathnames "images/icons/go-down.png" base)))
  (gp:register-image-translation 'img-go-up (gp:read-external-image (merge-pathnames "images/icons/go-up.png" base)))
  (gp:register-image-translation 'img-filenew (gp:read-external-image (merge-pathnames "images/icons/filenew.png" base)))
  (gp:register-image-translation 'img-fileopen (gp:read-external-image (merge-pathnames "images/icons/fileopen.png" base)))
  (gp:register-image-translation 'img-filesave (gp:read-external-image (merge-pathnames "images/icons/filesave.png" base)))
  (gp:register-image-translation 'img-filesaveas (gp:read-external-image (merge-pathnames "images/icons/filesaveas.png" base)))
  (gp:register-image-translation 'img-email (gp:read-external-image (merge-pathnames "images/icons/mail-forward.png" base))))

(defparameter *screen-width* nil)
(defparameter *screen-height* nil)
