(defsystem :cogworld.core-generic
  :description "Cognitive Science/Engineering Experiment Management System"
  :maintainer "Ryan Hope <hoper2@rpi.edu>"
  :version "2.0"
  :licence "LGPL-2.1"
  :serial t
  :components (
               (:file "misc")
               (:file "params")
               (:file "classes")
               (:file "aes8")
               (:file "string-utils")
	       #+MACOSX (:file "foreign-macosx")
	       #+WIN32 (:file "foreign-win32")
               )
  )
