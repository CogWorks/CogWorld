;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename     : eyetrack.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author       : Chris R. Sims
;; Copyright    : (C) 2005 Chris R. Sims
;; Address      : Cognitive Science Department
;;              : Rennselaer Polytechnic Institute
;;              : Troy, NY 12180
;;              : simsc@rpi.edu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Version      : 2.0.0
;; Description  : Code for communicating with LC Systems Eyegaze eyetracker
;;  via TCP/IP connection
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; History
;;
;; [2005.03.10] : File created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                    Constants & variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defconstant *eg-protocol*
  '((:eg-gaze-info    .  0)
    (:eg-calibrate    .  10) ;; To server
    (:eg-ws-query     .  11)
    (:eg-ws-response  .  12)
    (:eg-clear-screen .  13)
    (:eg-set-color    .  14)
    (:eg-set-diameter .  15)
    (:eg-draw-circle  .  16)
    (:eg-draw-cross   .  17)
    (:eg-display-text .  18)
    (:eg-cal-complete .  19)
    (:eg-cal-aborted  .  20)
    (:eg-begin-data   .  30) ;; To server
    (:eg-stop-data    .  31) ;; To server
    (:eg-disconnect   .  32)
    ))

(defparameter *eg-sample-x* 0)
(defparameter *eg-sample-y* 0)

(defparameter +tcp-address+ "192.168.2.2")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                      Connection handling ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmethod connect-eyetracker ((cw cogworld))
  (with-slots (eyetracker) cw
    (setf eyetracker (make-instance 'eyetracker-class :tcp-address +tcp-address+))
    (setf (io-stream eyetracker)
          (comm:open-tcp-stream (tcp-address eyetracker) (tcp-port eyetracker)
           :direction :io :timeout 5))
    (while (and (null (io-stream eyetracker))
                (capi:prompt-for-confirmation  
                 (format nil "Unable to connect to eyetracker on ~S. ~%Try again?" 
                                                       (tcp-address eyetracker) )))
           (setf (io-stream eyetracker)
          (comm:open-tcp-stream (tcp-address eyetracker) (tcp-port eyetracker)
           :direction :io :timeout 5)))    
    (when (and (io-stream eyetracker) (open-stream-p (io-stream eyetracker)))
         (send-message (io-stream eyetracker) :command :eg-calibrate)
          (setf (execution-process eyetracker)
                (mp:process-run-function
                 "Eyetracker read-loop"
                 nil 'read-loop))
          (mp:process-wait
           "Waiting for calibration to complete"
           #'(lambda ()
               (or
                (eq (status eyetracker) :calibration-complete)
                (eq (status eyetracker) :calibration-aborted)
                (eq (status eyetracker) :halted))))
          t)))

(defun eye-tracking-p ()
  (eyetracker *cw*))


(defmethod disconnect-eyetracker ((cw cogworld))
  (with-slots (eyetracker) cw
    (cond
     ((and eyetracker (io-stream eyetracker)
           (streamp (io-stream eyetracker))
           (open-stream-p (io-stream eyetracker)))
      (if (execution-process eyetracker)
          (mp:process-kill (execution-process eyetracker)))
    ;(write-char #\X (io-stream eyetracker))
    ;(write-char #\space (io-stream eyetracker))
    ;(force-output (io-stream eyetracker))
      (send-message (io-stream eyetracker) :command :eg-disconnect)
      (sleep 1)
      (close (io-stream eyetracker))
      (setf (io-stream eyetracker) nil)
      (setf (status eyetracker) :halted)))))

(defun start-eyetracking (&optional (reason ""))
  (when (eye-tracking-p)
  (send-message (io-stream (eyetracker *cw*)) :command :eg-begin-data)
  (log-info (list "EG-EVENT" "START-EYETRACKING" reason))
  (log-info (list "EG-EVENT" "EG-HEADER" "STATUS" "PUPIL DIAM"
                  "GAZE X" "GAZE Y" "FIELD COUNT" "EG-TIMESTAMP" "MOUSE X" "MOUSE Y" "BUTTON-STATE"))))

(defun stop-eyetracking (&optional (reason ""))
  (when (eye-tracking-p)
    (send-message (io-stream (eyetracker *cw*)) :command :eg-stop-data)
    (log-info (list "EG-EVENT" "STOP-EYETRACKING" reason))))

(defun turn-on-eyetracker-logging ()
  (if (eyetracker *cw*)
      (setf (logging (eyetracker *cw*)) t)))

(defun turn-off-eyetracker-logging ()
  (if (eyetracker *cw*)
      (setf (logging (eyetracker *cw*)) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         Message handling ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun send-message (stream &key command (body ""))
  (let ((command-val (assoc command *eg-protocol*)))
    (cond 
     ((not command-val)
      (stop-experiment *cw*)
      (error (format nil "EYETRACKER: SEND-MESSAGE: Unknown command: ~a.~%" command)))
     ((not (and (eyetracker *cw*) (io-stream (eyetracker *cw*))
                (streamp (io-stream (eyetracker *cw*)))
                (open-stream-p (io-stream (eyetracker *cw*)))))
      ;; Do nothing if stream is closed or doesn't exist
      )
     (t
      (setf command-val (cdr command-val))
      (format stream (format-message command-val body))
      ;(push (cons command body) (data-out (eyetracker *cw*)))
      (force-output stream)))))

(defun eyetracker-read-message (stream)
  (unless (or
           (eq (status (eyetracker *cw*)) :halted)
           (not stream)
           (not (open-stream-p stream))
           (not (ignore-errors (peek-char nil stream nil nil nil))))
    (let* ((accum 0)  (chk-sum 0)
           (h1 (char-int (read-char stream)))
           (h2 (char-int (read-char stream)))
           (h3 (char-int  (read-char stream)))
           (command (char-int  (read-char stream)))
           (body-length (- (+ (* h1 65536) (* h2 256) h3) 4))
           (body nil))
      (setq accum (+ h1 h2 h3 command))
      (when (> body-length 1)
        (setq body (make-string (- body-length 1)))
        (dotimes (i (- body-length 1))
          (incf accum (char-int (setf (elt body i) (read-char stream))))))
      (setq chk-sum (char-int (read-char stream)))
      (if (not (= chk-sum (logand #xff accum)))
          (error "EYEGAZE: READ-MESSAGE: Checksum doesn't match message."))
      (if (not (rassoc command *eg-protocol*))
          (error (format nil "EYEGAZE: READ-MESSAGE: Unknown command: ~a" command)))
      (values (car (rassoc command *eg-protocol*)) body)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                        Message execution ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;


(defmethod eg-gaze-info (status (pupil FIXNUM) (x FIXNUM) (y FIXNUM) (fc FIXNUM) ts &rest args)
  (setf *eg-sample-x* x)
  (setf *eg-sample-y* y)
  (log-info (append
             (list "EG-EVENT" "EG-DATA"
                   status pupil x y fc ts)
             (get-mouse-position)
             (list (get-button-state))))
  )



(defun read-loop ()
  (let ((eg-diameter 0)
        (eg-color nil)
        (pane (draw-pane (background-window *cw*)))
        (stream (io-stream (eyetracker *cw*))))
    
    (loop
     (when (or (null (io-stream (eyetracker *cw*)))
               (not (open-stream-p (io-stream (eyetracker *cw*))))
               (eq (status (eyetracker *cw*)) :calibration-aborted)
               (eq (status (eyetracker *cw*)) :halted)
               ) (return))

     (multiple-value-bind (command body) (eyetracker-read-message stream)
       ;(if command (push (cons command body) (data-in (eyetracker *cw*))))
       (capi:apply-in-pane-process pane #'(lambda () (gp:invalidate-rectangle pane)))
       (case command
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (:eg-gaze-info

          (eg-gaze-info 
           (parse-integer (subseq body 11 12))
           (parse-integer (subseq body 8 11))
           (parse-integer (subseq body 0 4))
           (parse-integer (subseq body 4 8))
           (parse-integer (subseq body 12 14)) ;; 
           (parse-integer (subseq body 14 24)) ;; [10 bytes] Timestamp
           )

          )
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (:eg-ws-query
          (send-message stream :command :eg-ws-response
                        :body (format nil "~4d,~4d,~4d,~4d,~4d,~4d,~4d,~4d"
                                      340 272
                                      (capi:screen-width (capi:convert-to-screen))
                                      (capi:screen-height (capi:convert-to-screen))
                                      (capi:screen-width (capi:convert-to-screen))
                                      (capi:screen-height (capi:convert-to-screen))
                                      0 0 )))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (:eg-clear-screen
          (setf (draw-commands (eyetracker *cw*)) nil)
          (gp:clear-graphics-port pane)
          )
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (:eg-set-color
          (let ((red (float (/ (char-int (elt body 2)) 255)))
                (green (float (/ (char-int (elt body 1)) 255)))
                (blue (float (/ (char-int (elt body 0)) 255))))
            (if (= (+ red green blue) 0.0)
                (setf eg-color :black)
              (setf eg-color (color:make-rgb red green blue 1.0)))
            ))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (:eg-set-diameter
          (let ((diam (+ (* (char-int (elt body 0)) 256) (char-int (elt body 1)))))
            (setf eg-diameter diam)
            ))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (:eg-draw-cross
          (let ((x (+ (* (char-int (elt body 0)) 256) (char-int (elt body 1))))
                (y (+ (* (char-int (elt body 2)) 256) (char-int (elt body 3)))))
            (push (cons :eg-draw-cross (list x y eg-diameter eg-color)) (draw-commands (eyetracker *cw*)))
            ))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (:eg-draw-circle
          (let ((x (+ (* (char-int (elt body 0)) 256) (char-int (elt body 1))))
                (y (+ (* (char-int (elt body 2)) 256) (char-int (elt body 3)))))
            (push (cons :eg-draw-circle (list x y eg-diameter eg-color)) (draw-commands (eyetracker *cw*)))
            ))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (:eg-display-text
         (let ((x (+ (* (char-int (elt body 0)) 256) (char-int (elt body 1))))
               (y (+ (* (char-int (elt body 2)) 256) (char-int (elt body 3))))
               (text (subseq body 4)))
           (push (cons :eg-display-text (list text x y eg-diameter eg-color))
                 (draw-commands (eyetracker *cw*)))
           ))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (:eg-cal-complete
         (setf (draw-commands (eyetracker *cw*)) nil)
         (setf (status (eyetracker *cw*)) :calibration-complete)
         )
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (:eg-cal-aborted
         (setf (status (eyetracker *cw*)) :calibration-aborted)
         ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          Screen-drawing  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               Utilities  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun checksum (&key header body)
  (let ((msg (concatenate 'string header body))
        (chksum 0))
    (dotimes (i (length msg))
      (incf chksum (char-int (elt msg i))))
    (format nil "~C" (int-char (logand chksum #xff)))))

(defun format-message (command &optional (body ""))
  (let* ((message-length (+ 5 (length body)))
         (header (multiple-value-bind (i1 r1) (floor message-length 65536)
                   (multiple-value-bind (i2 r2) (floor r1 256)
                     (format nil "~C~C~C~C" (int-char i1) (int-char i2) (int-char r2)
                             (int-char command))))))
    (concatenate 'string header body (checksum :header header :body body))))

(defun explode (string)
  (let* ((trimmed-string (string-trim '(#\Space) string))
         (i (position #\Space trimmed-string)))
    (if i
        (cons (subseq trimmed-string 0 i) (explode (subseq trimmed-string i)))
      (cons trimmed-string nil))))