(defsystem :cogworld.gui
  :description "Cognitive Science/Engineering Experiment Management System"
  :maintainer "Ryan Hope <hoper2@rpi.edu>"
  :version "2.0"
  :licence "LGPL-2.1"
  :serial t
  :depends-on (:cogworld.core-lispworks :cogworld.hardware)
  :components (
               (:file "splash-win")
               (:file "password-win")
               (:file "control-win")
               (:file "listener-win")
               (:file "subject-win")
               (:file "background-win")
               (:file "eyegaze-win")
               (:file "toolbar")
               #+MACOSX(:file "movie-view")
               )
  )
