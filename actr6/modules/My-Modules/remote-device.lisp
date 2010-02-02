(require "comm")

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

(defmacro conv-to-list (x)
  `(if ,x (ignore-errors (read-from-string (concatenate 'string "(" (string-trim '(#\Space #\. #\?) ,x) ")")))))

(defmacro error-message (txt)
  `(format (aif (actr-stream (remote-device)) it t) ,txt))

(defmacro defclassic (class supers &rest slots) 
  `(defclass 
	,class 
	,supers    
	,(mapcar 
	  #'(lambda (s)                   ; s  = slot specification 
	     (flet ((build (sn)           ; sn = slot name  
		      (list sn ':accessor sn ':initarg
			    (read-from-string                       
			     (concatenate 'string ":" (symbol-name sn))))))
       
	       (cond ((atom s) (append (build s) '(:initform nil)))
                     ((null (cddr s)) (append (build (first s)) (list ':initform (second s))))
                     ((eql t (second s))
                      (append (build (first s)) 
                              (nthcdr 2 s)
                              (if (not (member :initform (nthcdr 2 s))) '(:initform nil))))
                     (t s))))
	  slots) ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclassic remote-device () 
 actr-proc
 (actr-stream *standard-output*)
 cur-loc  
 (cur-cursor (vector 0 0))
 objects
 comm)


(defclassic Comm-object ()
  file-fs
  msgs-in
  msgs-out
  ip ;"127.0.0.1"
  (line-count 0)
  (to-device 3000 )
  (from-device 3001) ;;
  listen-proc
  listen-stream
  socket-proc
  write-stream
  enabled
 ) 

(defmethod initialize-instance :after ((dev remote-device) &rest args) 
  (init-comm (comm dev)))

;;;;;;;;initization and I/O routines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-stream-and-listen (handle)
  (let ((obj (comm (remote-device))))
    (with-slots (listen-stream listen-proc enabled) obj
      (setf listen-stream  (make-instance 'comm:socket-stream
                               :socket handle
                               :direction :io ;:input
                               :element-type 'base-char))
      (setf enabled t)
      (setf listen-proc (mp:process-run-function  "Server-Thread" '()  'read-process obj)))))

(defmethod read-process ( (obj comm-object))
  (with-slots (listen-stream  enabled msgs-in line-count) obj
    (while enabled 
           (let ((ln (ignore-errors (read-line listen-stream))))
             (when ln
               (push (list ln (get-internal-real-time))  msgs-in)
               (incf line-count)
               (msg-from (remote-device) (conv-to-list ln)))))))

(defmethod write-to-stream ((stream comm:socket-stream) text)
  (format stream text)
  (format stream "~%") ;(format stream "~C" #\NULL)
  (force-output stream))



(defmethod init-comm ((obj comm-object))
  (with-slots (from-device socket-proc) obj
      (setf socket-proc  (comm:start-up-server :function 'make-stream-and-listen :service  from-device))
      ))

(defmethod comm-end ((obj comm-object))
  (with-slots (enabled listen-proc socket-proc) obj
    (mp:process-kill socket-proc)
    (mp:process-kill listen-proc)
    (setf enabled nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;   Messages  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(defmethod send-to ((dev remote-device) msg &rest args)
  (with-slots (write-stream msgs-out ) (comm dev)
    (push msg msgs-out)  
    (case msg
      (:MOVE-CURSOR
       (write-to-stream write-stream (format nil "move-cursor ~A ~A" (first args) (second args)))
       )
      (:LEFT-CLICK-MOUSE
       (write-to-stream write-stream (format nil "left-hold-mouse ~A ~A" (first args) (second args)))
       (sleep .200)
       (write-to-stream write-stream (format nil "left-release-mouse ~A ~A" (first args) (second args)))
       )
      (:RIGHT-CLICK-MOUSE
     (write-to-stream write-stream (format nil "right-click-mouse ~A ~A" (first args) (second args)))
     )
      (:LEFT-HOLD-MOUSE
       (write-to-stream write-stream (format nil "left-hold-mouse ~A ~A" (first args) (second args)))
       )
      (:LEFT-RELEASE-MOUSE
       (write-to-stream write-stream (format nil "left-release-mouse ~A ~A" (first args) (second args)))
       )
      (:RIGHT-HOLD-MOUSE
       (write-to-stream write-stream (format nil "right-hold-mouse ~A ~A" (first args) (second args)))
       )
      (:RIGHT-RELEASE-MOUSE
       (write-to-stream write-stream (format nil "right-release-mouse ~A ~A" (first args) (second args)))
       )
      (:PRESS-KEY
       ;(error-message (format nil "press-key ~A" (first args)))
       (write-to-stream write-stream (format nil "press-key ~A" (first args)))
       )
      (:MOVE-ATTENTION
       (write-to-stream write-stream (format nil "move-attention ~A ~A" (first args) (second args)))
       )
    )))

(defmethod msg-from ((dev remote-device) msg)
  (with-slots (cur-cursor comm) dev
    (destructuring-bind (cmd &rest args) msg
      (case cmd
        (INIT
         (destructuring-bind (ip-in port) args
           (with-slots (write-stream ip to-device stream-type) comm
             (setf ip (remove #\| (write-to-string ip-in)) to-device port)
             (setf write-stream (comm:open-tcp-stream ip  to-device  :direction :output :element-type 'base-char)))))
        (MousePos
         (destructuring-bind ( x y) args
           (setf cur-cursor (vector x y))))
        ((AddObject UpdateObject RemoveObject)
         (process-remote-msg dev cmd (first args) (rest args)))
        (ProcessScreen
         (proc-display :clear nil)
         (print-visicon))
        (Model
         (case (first args) 
           (run (run-model))
           (stop
            (set-remote-stop))))
        (otherwise 
         (error-message (format nil "Remote Device received invalid message ~S ~S" msg args )))
        ))))

;;;;;;;;;;;;;;;;;;;;;;ACT-R Device Interface;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod device-move-cursor-to ((dev remote-device) xyloc)
  (format t "~%Moving mouse ~S ~S " xyloc (cur-cursor dev))
  (setf (cur-loc dev)  xyloc)
  (send-to dev :move-cursor (svref xyloc 0) (svref xyloc 1)))

(defmethod device-handle-click ((dev remote-device))
  (format t "~%Clicking mouse ~S  "  (cur-cursor dev))
  (send-to dev :left-click-mouse (svref (cur-cursor dev) 0 ) (svref (cur-cursor dev) 1 )))

(defmethod device-handle-keypress ((dev remote-device) key)
   (cond ((eql key #\q)
          (setq key #\newline)))
  (send-to dev :press-key key))

(defmethod get-mouse-coordinates ((dev remote-device))
  (cur-cursor dev) )

(defmethod device-speak-string ((dev remote-device) str))

(defmethod device-update-attended-loc ((dev remote-device) xyloc)
  (format t "~%Moving attention ~S "xyloc)
  (send-to dev :move-attention (svref xyloc 0) (svref xyloc 1)) 
  )
#|
(defmethod press-key ((mtr-mod motor-module) key)
  (when (stringp key)
    (if (eql (elt key 0) #\space)
        (setq key 'space)
      (setf key (read-from-string key))))
  (let ((command (key->cmd  (current-device-interface) key)))
    (apply (first command) mtr-mod (rest command))))
|#
(defmethod get-sub-objects ((dev remote-device))
  (objects dev))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-chunk-types ()
  (let ((res nil))
    (dolist (c (all-dm-chunks (get-module declarative)))
      (push (act-r-chunk-type-name (act-r-chunk-chunk-type (get-chunk c))) res))
    (remove-duplicates res)))

(defun get-production-names ()
 (mapcar (lambda(x) (production-name x)) (procedural-productions (get-module procedural))))

(defun get-production-by-name (name)
  (get-production  name))


(defun show-buffers (&optional (bufs (buffers)))
  (if (atom bufs)
    (buffer-chunk-fct (list bufs))
    (dolist (buf bufs)
      (buffer-chunk-fct (list buf)))))

(defun show-events ()
  (meta-p-events (get-mp (current-meta-process))))

(defun list-all-parameters ()
  (let ((current-val-table (make-hash-table))
        (res nil))
    (maphash #'(lambda (p-name param)
                 (push (cons param (process-parameters (act-r-parameter-owner param) p-name)) 
                       (gethash (act-r-parameter-owner param) current-val-table)))
             *act-r-parameters-table*)
    (maphash #'(lambda (module-name parameters) (declare (ignore module-name))
                 (dolist (param parameters)
                   (push (list (act-r-parameter-param-name (car param)) (cdr param)) res)))
             current-val-table)
    res))


(defmacro defp (&rest definition)
  "Production definition."
  `(p-fct ',definition))

(defun get-device () 
  (device (current-device-interface)))

(defvar *act-r-stream* *standard-output*)

(let ((remote-stop nil))

(defun set-remote-stop ()
  (setf remote-stop t))

(defun clear-remote-stop ()
  (setf remote-stop nil))

(defun remote-stop-p ()
  remote-stop))

(defun run-model (&key (run-time nil) (rt t))
  (let ((dev (get-device)))
    (when dev
      (clear-remote-stop)
      (if run-time (mp:process-run-function "actr" '() (lambda() (sleep run-time) (set-remote-stop))))
      (setf (actr-proc dev) 
            (mp:process-run-function "ACTR" '() 
              (lambda () (setf *standard-output* *act-r-stream*) ;;(actr-stream dev)) 
                         (run-until-condition 'remote-stop-p :real-time rt)) )))))

(defun quit-actr ()
  (mp:process-kill (actr-proc (remote-device))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((rd (make-instance 'remote-device :comm (make-instance 'comm-object))))
  (defun remote-device () rd)
  (defun start-comm () (init-comm (comm rd)))
  (defun get-comm () (comm rd)))







