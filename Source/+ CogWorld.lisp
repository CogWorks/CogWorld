;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename     : + CogWorld.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author       : Mike Schoelles, Chris R. Sims, Ryan M. Hope
;; Copyright    : (C) 2005, 2009-2010 CogWorks Laboratory
;; Address      : Cognitive Science Department
;;              : Rennselaer Polytechnic Institute
;;              : Troy, NY 12180
;;              : schoem@rpi.edu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export
 '(register-task mw-register-task configuration-done mw-configure-done
                 get-rin log-info graphics-feature *delivered* mw-speak
                 mw-interface mw-output-pane mw-pinboard-layout
                 monitor-region mouse-position update-screen mw-task-finished task-finished
                 mw-update-screen icon-feature task-conditions *task-conditions*
                 prompt-for-password *screen-width* *screen-height*))

(require "comm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File system definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem "CogWorld-Files"
  (:package CL-USER)
  :members 
#+MACOSX
           (
            "Modules/matlab-lispworks.lisp"
            "Modules/control.lisp"
            "Modules/eyegaze.lisp"
            "Modules/foreign.lisp"
            "Modules/event.lisp"
            "Modules/response-pad.lisp"
            "Modules/color-vision.lisp"
             "Modules/aes8.lisp"
            "Modules/SendMail.lisp"
            
            "Modules/logging.lisp"
            "Modules/remote-app.lisp"
           
            "Modules/eeg.lisp"
            "GUI/splash-win.lisp"
            "GUI/password-win.lisp"
            "GUI/control-win.lisp"
            "GUI/listener-win.lisp"
            "GUI/subject-win.lisp"
            "GUI/background-win.lisp"
            "GUI/eyegaze-win.lisp"
            "GUI/toolbar.lisp"
            "GUI/movie-view.lisp"
            )

#+WIN32
          (
            "Modules/matlab-lispworks.lisp"
            "Modules/control.lisp"
            "Modules/eyegaze.lisp"
            "Modules/foreign-win32.lisp"
            "Modules/event.lisp"
            "Modules/color-vision.lisp"
            "Modules/sendmail.lisp"
            "Modules/aes8.lisp"
            "Modules/logging.lisp"
            "Modules/remote-app.lisp" 
            "GUI/splash-win.lisp"
            "GUI/password-win.lisp"
            "GUI/control-win.lisp"
            "GUI/listener-win.lisp"
            "GUI/subject-win.lisp"
            "GUI/background-win.lisp"
            "GUI/eyegaze-win.lisp"
            "GUI/toolbar.lisp"
            )
  :rules  ((:in-order-to :compile :all
            (:requires (:load :previous)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
     (progn ,@body)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provided for backward compatability: use :COGWORLD
(if (not (find :MULTIWORLD *features*))
    (push :MULTIWORLD *features*))

(if (not (find :COGWORLD *features*))
    (push :COGWORLD *features*))

(defparameter *version-string* "1.2")

(defparameter *default-experiment-settings-file-directory* (concatenate 'string (format nil "~a" (sys:get-folder-path :documents)) "CogWorld/"))

(defparameter *mw* nil) ;; Provided for backward compatability: use *cw*
(defparameter *cw* nil)
(defparameter *delivered*
  #+:delivered t
  #-:delivered nil)

(defparameter *experiment-settings-file* nil)

(defparameter *task-conditions* nil)

;; Image files
(defparameter *splash* (gp:read-external-image (current-pathname "Images/new-splash.bmp")))
(defparameter *rpi* (gp:read-external-image (current-pathname "Images/RPI.bmp")))
(defparameter *cogworks* (gp:read-external-image (current-pathname "Images/CogWorks.bmp")))
(defparameter *sponsors* (gp:read-external-image (current-pathname "Images/Sponsors.bmp")))
(defparameter *color-test* nil)

(defparameter *screen-width* nil)
(defparameter *screen-height* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cogworld ()
  ((local-path :initform nil :initarg :local-path :accessor local-path)
   (cw-path :initform (current-pathname) :accessor cw-path)
   (experiment-name :initform "UNTITLED" :initarg :experiment-name :accessor experiment-name)
   (experiment-version :initform 1 :initarg :experiment-version :accessor experiment-version)
   (subject-info :initform nil :initarg :subject-info :accessor subject-info)
   (task-list :initform nil :initarg :task-list :accessor task-list)
   (current-task :initform 0 :initarg :current-task :accessor current-task)
   (status :initform nil :initarg :status :accessor status)
   (control-mode :initform nil :initarg :control-mode :accessor control-mode)
   (dispatched-configs :initform 0 :initarg :dispatched-configs :accessor dispatched-configs)
   (task-condition-list :initform nil :accessor task-condition-list)
   (hot-regions :initform nil :initarg :hot-regions :accessor hot-regions)
   (startup-process :initform nil :initarg :startup-process :accessor startup-process)
   (monitor-process :initform nil :initarg :monitor-process :accessor monitor-process)
   (eyetracker :initform nil :initarg :eyetracker :accessor eyetracker)
   (logging :initform nil :initarg :logging :accessor logging)
   (control-window :initform nil :initarg :control-window :accessor control-window)
   (subject-window :initform nil :initarg :subject-window :accessor subject-window)
   (listener-window :initform nil :initarg :listener-window :accessor listener-window)
   (background-window :initform nil :initarg :background-window :accessor background-window)
   (eyegaze-window :initform nil :initarg :eyegaze-window :accessor eyegaze-window)
   (color-task :initform nil :initarg :color-task :accessor color-task)
   (run-proc :initform nil :initarg :run-proc :accessor run-proc)))

(defclass task-class ()
  ((name :initform nil :initarg :name :accessor name)
   (app :initform 'lisp :initarg :app :accessor app) 
   (path :initform nil :initarg :path :accessor path)
   (task-condition :initform "?" :initarg :task-condition :accessor task-condition)
   (process :initform nil :initarg :process :accessor process)
   (status :initform nil :initarg :status :accessor status)
   (run-function :initform nil :initarg :run-function :accessor run-function)
   (model-run-function :initform nil :initarg :model-run-function :accessor model-run-function)
   (configure-function :initform nil :initarg :configure-function :accessor configure-function)
   (break-function :initform nil :initarg :break-function :accessor break-function)
   (replay-function :initform nil :initarg :replay-function :accessor replay-function)))

(defclass subject-info-class ()
  ((first-name :initform "??" :initarg :first-name :accessor first-name)
   (last-name :initform "??" :initarg :last-name :accessor last-name)
   (rin :initform "000000" :initarg :rin :accessor rin)
   (age :initform "??" :initarg :age :accessor age)
   (gender :initform "??" :initarg :gender :accessor gender)
   (major :initform "??" :initarg :major :accessor major)
   (date :initform "??" :initarg :date :accessor date)
   (uid :initform nil :accessor uid)
   (exp-history :initform nil :accessor exp-history :initarg :exp-history)
   (num-blocks :initform nil :initarg num-blocks :accessor num-blocks)
   (num-trials :initform nil :initarg num-blocks :accessor num-trials)))

(defclass eyetracker-class ()
  ((status :initform nil :initarg :status :accessor status)
   (logging :initform t :initarg :logging :accessor logging)
   (draw-commands :initform nil :initarg :draw-commands :accessor draw-commands)
   (tcp-port :initform 3999 :initarg :tcp-port :accessor tcp-port)
   (tcp-address :initform "1.0.0.3" :initarg :tcp-address :accessor tcp-address)
   (io-stream :initform nil :initarg :io-stream :accessor io-stream)
   (execution-process :initform nil :accessor execution-process)
   (data-in :initform nil :initarg :data-in :accessor data-in)
   (data-out :initform nil :initarg :data-out :accessor data-out)))

(defclass color-task ()
  ((tite :accessor task-title :initform "Ishihara")
   (color-test-window :initform nil :initarg :color-test-window :accessor color-test-window)
   (center-x :accessor task-center-x :initform (floor *screen-width* 2))    ; x coordinate of center
   (center-y :accessor task-center-y :initform (floor *screen-height* 2))   ; y coordinate of center
   (color-test-images :accessor color-test-images :initform nil :initarg :color-test-images)
   (color-test-list :initform (append '((1 12 12)) (permute-lst '((2 8 3) (3 29 70) (4 5 3) (5 3 5) (6 15 17) (7 74 21) (8 6 ?) (14 ? 5) (15 ? 45)))) :accessor color-test-list)
   ))



(defclass logging-class ()
  ((file-path :initform nil :initarg :file-path :accessor file-path)
   (file-handle :initform nil :initarg :file-handle :accessor file-handle)
   (data-out :initform nil :initarg :data-out :accessor data-out)
   (resource-lock :initform (mp:make-lock) :accessor resource-lock)))

(defclass mw-interface (capi:interface)
  ((parse :initform t :initarg :parse :accessor parse)))

(defclass mw-output-pane (capi:output-pane)
  ((parse-callback :initform nil :initarg :parse-callback :accessor parse-callback)))

(defclass mw-pinboard-layout (capi:pinboard-layout)
  ((parse-callback :initform nil :initarg :parse-callback :accessor parse-callback)))

(defclass hot-region ()
  ((x1 :initform nil :initarg :x1 :accessor x1)
   (x2 :initform nil :initarg :x2 :accessor x2)
   (y1 :initform nil :initarg :y1 :accessor y1)
   (y2 :initform nil :initarg :y2 :accessor y2)
   (entered :initform nil :initarg :entered :accessor entered)
   (callback :initform nil :initarg :callback :accessor callback)
   (callback-type :initform :keyword :initarg :callback-type :accessor callback-type)))

(defclass mw-device ()
  ((show-focus-p :initform t :accessor show-focus-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(compile-system "CogWorld-Files" :force t)
(load-system "CogWorld-Files")

(if (not *delivered*)
    (build-world))

(provide "CogWorld")
