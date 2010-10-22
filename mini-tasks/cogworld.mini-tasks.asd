(defsystem :cogworld.mini-tasks
  :description "Cognitive Science/Engineering Experiment Management System"
  :maintainer "Ryan Hope <hoper2@rpi.edu>"
  :version "2.0"
  :licence "LGPL-2.1"
  :serial t
  :depends-on (:cogworld.gui)
  :components (
               (:file "color-vision")
               (:file "edinburgh")
               )
  )