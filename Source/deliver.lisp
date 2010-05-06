(in-package "CL-USER")
(load-all-patches)
(push ':DELIVERED *features*)

(load (current-pathname "+ CogWorld.lisp"))

;; Load the example code which creates an Application Bundle
#+:cocoa
(compile-file-if-needed
(sys:example-file "configuration/macos-application-bundle")
:load t)
;; Now deliver the application itself and create the 
;; application

(deliver 'build-world
         #+:cocoa 
         (CL-USER::write-macos-application-bundle
          "/Applications/CogWorld\ 1.2/CogWorld.app"
          :DOCUMENT-TYPES NIL)
         #-:cocoa "CogWorld"
         0 :interface :capi)

(quit)
