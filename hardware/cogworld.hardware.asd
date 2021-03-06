(defsystem :cogworld.hardware
  :description "Cognitive Science/Engineering Experiment Management System"
  :maintainer "Ryan Hope <hoper2@rpi.edu>"
  :version "2.0"
  :licence "LGPL-2.1"
  :serial t
  :depends-on (:cogworld.core-generic :cl-json)
  :components (
               (:file "eeg")
               (:file "eyegaze")
               (:file "response-pad")
	       (:file "json-rpc-server")
               )
  )
