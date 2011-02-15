;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CogWorld JSON RPC Server v1.0
;;  Maintained by: Ryan Hope <rmh3093@gmail.com>
;;
;;  Notes:
;;   1) All lisp rpc methods are converted to camel-case notation
;;      (ex. 'cw-get-version' = 'cwGetVersion')
;;   2) All rpc calls must include a 'method' key and an 'id' key,
;;      the 'params' key is optional.
;;
;;  Client Example Calls:
;;   1) {"method":"cwGetVersion","id":"SpaceFortress"}
;;   2) {"method":"cwLogInfo","params":[["foo","bar","baz"]],"id":"williams67"}
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  RPC Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(json-rpc:defun-json-rpc cw-get-version :streaming ()
  (format nil "\"~A\"" *version-string*))

(json-rpc:defun-json-rpc cw-eeg-begin-record :streaming ()
  (eeg-proc 'begin-record)
  0)

(json-rpc:defun-json-rpc cw-eeg-end-record :streaming ()
  (eeg-proc 'end-record)
  0)

(json-rpc:defun-json-rpc cw-eeg-event-notify :streaming (duration type-code label data)
  (eeg-proc 'event-notify 1 :duration duration :type-code type-code :label label :data data)
  0)

(json-rpc:defun-json-rpc cw-log-info :streaming (list)
  (if (listp list)
      (progn
        (log-info list)
        0)
    1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-stream-and-talk (handle)
  (let ((stream (make-instance 'comm:socket-stream
                               :socket handle
                               :direction :io
                               :element-type
                               'base-char)))
    (mp:process-run-function (format nil "talk ~D"
                                     handle)
                             '()
                             'talk-on-stream stream)))

(defun talk-on-stream (stream)
  (unwind-protect
      (loop for line = (read-line stream nil nil)
            while line
            do
            (handler-case
                (progn (json:with-decoder-simple-list-semantics
                         (format stream "~A~%" (json-rpc:invoke-rpc line))
                         (force-output stream))
                  t)
              (json:json-syntax-error ()
                ;; Malformed JSON
                t)
              (json-rpc:json-rpc-call-error ()
                ;; Method does not exist or missing params
                t)
              (end-of-file ()
                ;; Should probably close connection if this gets hit
                t)))
    (close stream)))

(defun json-rpc-server-announce (socket condition)
  (when socket
    (setf (json-rpc-server-socket *cw*) socket)))

(defun json-rpc-server-get-port ()
  (if (and *cw* (json-rpc-server-socket *cw*))
      (multiple-value-bind (address port)
          (comm:get-socket-address (json-rpc-server-socket *cw*))
        port)
    nil))

(defun json-rpc-server-start ()
  (setf json-rpc:*json-rpc-version* json-rpc:+json-rpc-2.0+)
  (comm:start-up-server :process-name "cw-json-rpc-server"
                        :function 'make-stream-and-talk
                        :announce 'json-rpc-server-announce
                        :service 0))

(defun json-rpc-server-stop ()
  (when (json-rpc-server-process *cw*)
    (mp:process-kill (json-rpc-server-process *cw*))))