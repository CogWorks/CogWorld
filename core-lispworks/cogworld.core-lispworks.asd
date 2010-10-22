(defsystem :cogworld.core-lispworks
  :description "Cognitive Science/Engineering Experiment Management System"
  :maintainer "Ryan Hope <hoper2@rpi.edu>"
  :version "2.0"
  :licence "LGPL-2.1"
  :serial t
  :depends-on (:cogworld.core-generic)
  :components (
               (:file "matlab-lispworks")
               (:file "control")
               (:file "event")
               (:file "logging")
               (:file "remote-app")
               )
  )