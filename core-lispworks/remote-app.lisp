(require "comm")

(defmacro error-message (txt)
  `(format (aif (output-stream (remote-app)) it t) ,txt))

(defclassic remote-app () 
; actr-proc
 (output-stream *standard-output*)
; cur-loc  
; (cur-cursor (vector 0 0))
; objects
 task-obj
 comm)


(defclassic Comm-object ()
  file-fs
  msgs-in
  msgs-out
  ip ;"127.0.0.1"
  (line-count 0)
  (to-device 3000 )
  (from-device 3001) 
  listen-proc
  listen-stream
  socket-proc
  write-stream
  enabled
 ) 

(defmethod initialize-instance :after ((dev remote-app) &rest args) 
  (init-comm (comm dev)))

;;;;;;;;initization and I/O routines;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-stream-and-listen (handle)
  (let ((obj (comm (remote-app))))
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
               (msg-from (remote-app) (conv-to-list ln)))))))



(defmethod init-comm ((obj comm-object))
  (with-slots (from-device socket-proc) obj
      (setf socket-proc  (comm:start-up-server :function 'make-stream-and-listen :service  from-device))
      ))

(defmethod comm-end ((obj comm-object))
  (with-slots (enabled listen-proc socket-proc) obj
    (mp:process-kill socket-proc)
    (mp:process-kill listen-proc)
    (setf enabled nil)))

(defmethod write-to-stream ((stream comm:socket-stream) text)
  (format stream text)
  (format stream "~%") ;(format stream "~C" #\NULL)
  (force-output stream))


;;;;;;;;;;;;;;;;;;;;;;;;;   Messages  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defmethod send-to ((dev remote-app) msg &rest args)
  (with-slots (write-stream msgs-out ) (comm dev)
    (push (cons msg args) msgs-out)  
    (case msg
      (:RUN
       (write-to-stream write-stream (format nil "~A" (first args))))
    )))


(defvar debuglog nil)
(defmethod msg-from ((dev remote-app) msg)
  (with-slots (task-obj comm) dev
    (destructuring-bind (cmd &rest args) msg
      (case cmd

        (INIT
         (destructuring-bind (ip-in port) args
           (with-slots (write-stream ip to-device stream-type) comm
             (setf ip (remove #\| (write-to-string ip-in)) to-device port)
             (setf write-stream (comm:open-tcp-stream ip  to-device  :direction :output :element-type 'base-char)))))
        (END (setf (write-stream comm) nil) (task-finished task-obj))
#|
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
|#
        (Loginfo
         ;(push msg debuglog)
         (let ((lst (conv-to-list (first args))))
           (log-info `(,(concatenate 'string (experiment-name *cw*) "-EVENT")  ,@lst ,(first (last args))))))
        (otherwise 
         (error-message (format nil "Remote Device received invalid message ~S ~S" msg args )))
        ))))

(let ((rd nil))
  (defun make-remote-app ()
    (setf rd (make-instance 'remote-app :comm (make-instance 'comm-object)))
    ;(setf (write-stream rd)  (comm:open-tcp-stream "127.0.0.1"  (to-device rd)  :direction :output :element-type 'base-char))
    )
  (defun remote-app () rd)
  (defun start-comm () (init-comm (comm rd)))
  (defun get-comm () (comm rd)))