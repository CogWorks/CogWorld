;; Qccc "MAC-" "UNIX" "NTEL"
;; B - Begin recording
;; E - End recording
;; X - Quit Net Station
;; Tllll - Time in milliseconds
;; Dllll - Signal event

;;; Responses
;; IB / Fss
;; Z / Fss

;; Connect on port 55513

(push :eeg *features*)

(if (not (find-package 'eeg)) (make-package 'eeg))
(in-package 'eeg)
(export '(socket closed-socket-p event connect sync-time begin-record end-record quit-eeg send-event disconnect     test-tcpip initialize uninitialize event-notify))

(defclass socket ()
  ((tcp-stream :initform nil :initarg :stream :accessor socket-stream)
   (status :initform nil :initarg :status :accessor socket-status)
   (error :initform 0 :initarg :error :accessor socket-error)))

(defmethod closed-socket-p ((s socket))
  (if (open-stream-p (socket-stream s))
      nil
    t))

(defclass event ()
  ((start :initform (get-internal-real-time) :initarg :start :accessor event-start)
   (duration :initform 1 :initarg :duration :accessor event-duration)
   (type-code :initform "EVNT" :initarg :type :accessor event-type)
   (label :initform "" :initarg :label :accessor event-label)
   (description :initform "" :initarg :description :accessor event-description)
   (data :initform nil :initarg :data :accessor event-data)))

(defun string-to-vector (str)
  (let ((vec (make-array (length str) :element-type 'unsigned-byte :initial-element 0)))
    (do ((i 0 (1+ i)))
        ((= i (length str)) vec)
      (setf (elt vec i) (char-code (elt str i))))))

(defun ushort-to-vector (i)
  (if (or (< i 0) (> i 65536)) (error "i was not in the valid range for unsigned short numbers."))
  (let ((vec (make-array 2 :initial-element 0 :element-type 'unsigned-byte)))
    (setf (elt vec 1) (mod i 256))
    (setf (elt vec 0) (floor (/ i 256)))
    vec
))

(defun uint-to-vector (i)
  (if (or (< i 0) (> i 4294967295)) (error "i was not in the valid range for unsigned long numbers."))
  (let ((vec (make-array 4 :initial-element 0 :element-type 'unsigned-byte)))
    (setf (elt vec 3) (mod i 256))
    (setq i (floor (/ i 256)))
    (setf (elt vec 2) (mod i 256))
    (setq i (floor (/ i 256)))
    (setf (elt vec 1) (mod i 256))
    (setf (elt vec 0) (floor (/ i 256)))
    vec
))

(defun code-to-vector (c)
  (let ((vec (make-array 4 :initial-element (char-code #\space) :element-type 'unsigned-byte))
        (len (min (length c) 4)))
    (do ((i 0 (1+ i)))
        ((= i len) vec)
      (setf (elt vec i) (char-code (elt c i))))))

(let ((last-stream nil))

(defun get-response (strm)
  (let ((start (read-byte strm)))
    (cond ((char= (code-char start) #\Z) (return-from get-response 'Z))
          ((char= (code-char start) #\I) (read-byte strm) (return-from get-response 'I))
          ((char= (code-char start) #\F)
           (let ((b1 (read-byte strm))
                 (b2 (read-byte strm)))
             (return-from get-response (+ (* 256 b2) b1))))
          (t
           (error "EEG system returned unknown response starting with byte ~A" start)))))

(defun connect (ip &optional (port 55513) &key (os "MAC-") (sync t))
  (let ((strm (comm:open-tcp-stream ip port :direction :io :errorp nil :element-type 'unsigned-byte))
        (sock nil)
        (res nil))
    (if (null strm) (return-from connect (values nil nil)))
    (setq sock (make-instance 'socket :stream strm :status 'connected))
    (write-sequence (string-to-vector (format nil "Q~A" os)) strm)
    (force-output strm)
    (setq res (get-response strm))
    (cond ((numberp res)
           (setq last-stream nil)
           (setf (socket-status sock) 'error)
           (setf (socket-error sock) res)
           (close (socket-stream sock))
           (return-from connect (values sock nil)))
          (t
           (setq last-stream sock)))
    (if sync
        (setq sync (sync-time)))
    (values sock sync)
  ))

(defun disconnect (&optional eeg)
  (if (or (null eeg) (closed-socket-p eeg)) (setq eeg last-stream))
  (if (or (null eeg) (closed-socket-p eeg)) (error "Must supply a valid EEG socket to begin recording."))
  (if (equal 'recording (socket-status eeg))
      (end-record eeg))
  (close (socket-stream eeg))
  (setf (socket-status eeg) 'closed)
  (setf (socket-error eeg) 0)
  (setq last-stream nil)
  eeg)

(defun sync-time (&optional time eeg)
  (if (or (null eeg) (closed-socket-p eeg)) (setq eeg last-stream))
  (if (or (null eeg) (closed-socket-p eeg)) (error "Must supply a valid EEG socket to begin recording."))
  (if (null time) (setq time (get-internal-real-time)))
  (let ((strm (socket-stream eeg))
        (vec (make-array 5 :initial-element (char-code #\T) :element-type 'unsigned-byte))
        (res nil))
    (setf (subseq vec 1 5) (uint-to-vector time))
    (write-sequence vec strm)
    (force-output strm)
    (setq res (get-response strm))
    (cond ((numberp res)
           (setq last-stream nil)
           (setf (socket-status eeg) 'error)
           (setf (socket-error eeg) res)
           (close (socket-stream eeg))
           (return-from sync-time nil))
          (t
           (setq last-stream eeg)))
    time))

(defun begin-record (&optional eeg)
  (if (or (null eeg) (closed-socket-p eeg)) (setq eeg last-stream))
  (if (or (null eeg) (closed-socket-p eeg)) (error "Must supply a valid EEG socket to begin recording."))
  (let ((strm (socket-stream eeg))
        (res nil))
    (write-sequence (string-to-vector "B") strm)
    (force-output strm)
    (setq res (get-response strm))
    (cond ((numberp res)
           (setq last-stream nil)
           (setf (socket-status eeg) 'error)
           (setf (socket-error eeg) res)
           (close (socket-stream eeg))
           (return-from begin-record res))
          (t
           (setq last-stream eeg)))
    (setf (socket-status eeg) 'recording)
    nil
  ))

(defun end-record (&optional eeg)
  (if (or (null eeg) (closed-socket-p eeg)) (setq eeg last-stream))
  (if (or (null eeg) (closed-socket-p eeg)) (error "Must supply a valid EEG socket to end recording."))
  (let ((strm (socket-stream eeg))
        (res nil))
    (write-sequence (string-to-vector "E") strm)
    (force-output strm)
    (setq res (get-response strm))
    (cond ((numberp res)
           (setq last-stream nil)
           (setf (socket-status eeg) 'error)
           (setf (socket-error eeg) res)
           (close (socket-stream eeg))
           (return-from end-record res))
          (t
           (setq last-stream eeg)))
    (setf (socket-status eeg) 'connected)
    nil
  ))

(defun quit-eeg (&optional eeg)
  (if (or (null eeg) (closed-socket-p eeg)) (setq eeg last-stream))
  (if (or (null eeg) (closed-socket-p eeg)) (error "Must supply a valid EEG socket to stop Net Station."))
  (let ((strm (socket-stream eeg))
        (res nil))
    (write-sequence (string-to-vector "X") strm)
    (force-output strm)
    (setq res (get-response strm))
    (cond ((numberp res)
           (setq last-stream nil)
           (setf (socket-status eeg) 'error)
           (setf (socket-error eeg) res)
           (close (socket-stream eeg))
           (return-from quit-eeg res))
          (t
           (setq last-stream eeg)))
    nil
  ))

(defun send-event (event &optional eeg)
  (if (or (null eeg) (closed-socket-p eeg)) (setq eeg last-stream))
  (if (or (null eeg) (closed-socket-p eeg)) (error "Must supply a valid EEG socket to send signal."))
  (let* ((label-length (length (event-label event)))
         (desc-length (length (event-description event)))
         (data-string (if (event-data event) (format nil "~S" (event-data event))))
         (data-length (length data-string))
         (struct-length (+ 25 label-length desc-length data-length))
         (msg (make-array (+ 3 struct-length) :initial-element 0 :element-type 'unsigned-byte))
         (i 0)
         (res nil)
         (strm (socket-stream eeg)))
    (setf (elt msg i) (char-code #\D))
    (incf i)
    (setf (subseq msg i (+ i 2)) (ushort-to-vector struct-length))
    (incf i 2)
    (setf (subseq msg i (+ i 4)) (uint-to-vector (event-start event)))
    (incf i 4)
    (setf (subseq msg i (+ i 4)) (uint-to-vector (event-duration event)))
    (incf i 4)
    (setf (subseq msg i (+ i 4)) (code-to-vector (event-type event)))
    (incf i 4)
    (setf (elt msg i) (coerce label-length 'unsigned-byte))
    (incf i)
    (cond ((< 0 label-length)
           (setf (subseq msg i (+ i label-length)) (string-to-vector (event-label event)))
           (incf i label-length)))
    (setf (elt msg i) (coerce desc-length 'unsigned-byte))
    (incf i)
    (cond ((< 0 desc-length)
           (setf (subseq msg i (+ i desc-length)) (string-to-vector (event-description event)))
           (incf i desc-length)))
    (setf (elt msg i) (coerce 0 'unsigned-byte))
    (incf i)
    (setf (subseq msg i (+ i 4)) (code-to-vector ""))
    (incf i 4)
    (setf (subseq msg i (+ i 4)) (code-to-vector ""))
    (incf i 4)
    (setf (subseq msg i (+ i 2)) (ushort-to-vector data-length))
    (incf i 2)
    (cond ((< 0 data-length)
           (setf (subseq msg i (+ i data-length)) (string-to-vector data-string))
           (incf i data-length)))
    (format t "~S" msg)
    (write-sequence msg strm)
    (force-output strm)
    (setq res (get-response strm))
    (cond ((numberp res)
           (setq last-stream nil)
           (setf (socket-status eeg) 'error)
           (setf (socket-error eeg) res)
           (close (socket-stream eeg))
           (return-from send-event res))
          (t
           (setq last-stream eeg)))
    nil
  ))
)

;; 1 byte 'D'
;; 2 byte length of following data
;; 4 byte start time
;; 4 byte duration
;; 4 byte event code
;; 1 byte label length
;; variable length label
;; 1 byte description length
;; variable length description
;; 1 byte number of keys
;; 4 byte key
;; 4 byte data type
;; 2 byte variable length data
;; variable length data
#|
(fli:register-module (probe-file "/usr/lib/libeeglib.dylib") :connection-style :immediate)

(fli:define-foreign-function
    (connect-din "ConnectDIN" :source)
    ()
  :result-type :int
  :language :ansi-c
  )

(fli:define-foreign-function
    (send-din-event "SendDINSignal" :source)
    ((value :int))
  :result-type :int
  :language :ansi-c
  )

(fli:define-foreign-function
    (get-din-event "GetDINSignal" :source)
    ()
  :result-type :int
  :language :ansi-c
  )

(fli:define-foreign-function
    (disconnect-din "DisconnectDIN" :source)
    ()
  :result-type :int
  :language :ansi-c
  )

(defun test-din ()
  (connect-din)
  (send-din-event 0)
  (dotimes (x 255)
    (send-din-event x)
    (sleep 1))
  (disconnect-din))
|#

(defun test-tcpip ()
  (initialize "1.0.0.4")
  (dotimes (x 20)
    (event-notify (* 10 x) :label "Hello")
    (sleep 5)))

(defun event-notify (eid &key (duration 1) (type-code nil) (label "") (description "") (data nil))
  (let ((tm (get-internal-real-time))
        (evt nil))
    ;(send-din-event eid)
    ;(let ((res (get-din-event)))
    ;  (if (/= res eid) (capi:display-message (format nil "Sent ~A, but bitwhacker shows ~A" eid res))))
    (setq evt (make-instance 'event :start tm :duration duration :type (if type-code type-code (format nil "D~A" eid)) :label label :description description :data data))
    (send-event evt)
   ; (send-din-event 0)
    evt))

(defun initialize (ip &optional (port 55513) &key (os "MAC-") (sync t))
  (connect ip port :os os :sync sync)
;  (connect-din)
  ;(send-din-event 0)
)

(defun uninitialize ()
  (disconnect)
;  (disconnect-din)
)