;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename     : logging.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author       : Mike Schoelles, Chris R. Sims
;; Copyright    : (C) 2005, 2009 CogWorks Laboratory
;; Address      : Cognitive Science Department
;;              : Rennselaer Polytechnic Institute
;;              : Troy, NY 12180
;;              : schoem@rpi.edu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Version      : 2.0.0
;; Description  : Code for opening, closing and writing to log files.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; History
;;
;; [2005.03.10] : File created
;; [2005.04.13] : Added a resource lock to deal with multiprocessing conflict
;;   issue (eyetracker & task both logging simultaneously).
;; [2009.08.24] : New privacy constraints for IRB 
;; [2009.08.24] : Converted to Cogworld
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun define-logging-folder (pathname)
  (setf (local-path *cw*) (probe-file pathname))
  (if (not (local-path *cw*))
      (capi:display-message "Error [define-logging-folder]: Could not locate folder ~a." pathname)))

;; Open a file for data logging.
(defmethod open-logging-file ((cw cogworld))
  (if (null (logging cw)) (setf (logging cw) (make-instance 'logging-class)))
  (let ((fn (aif (file-path (logging cw)) (merge-pathnames (make-pathname :type "log") it ) (logging-filename :ftype "log"))))
    (setf (file-handle (logging cw))
        (open fn :if-exists :supersede :direction :output))))

(defmethod create-history-file ((cw cogworld))
  (let ((fn (logging-filename :ftype "history")))
    (setf (logging cw) (make-instance 'logging-class))
    (setf (file-path (logging cw)) fn)))

(defmethod write-history-file ((cw cogworld) header values)
    (with-open-file (fs (file-path (logging cw)) :direction :output :if-exists :error)
      (dolist (item header)
        (write item :stream fs) (write-char #\tab fs))
      (write-char #\newline fs)   
      (dolist (item  values)
        (write item :stream fs) (write-char #\tab fs))
      (write-char #\newline fs)))

(defun open-file (filename)
  (open filename :if-exists :supersede :direction :output))

;; Close the file.
(defun close-logging-file ()
  (if (and (logging *mw*) (file-handle (logging *mw*))
           (open-stream-p (file-handle (logging *mw*))))
      (progn (force-output (file-handle (logging *mw*)))
        (close (file-handle (logging *mw*))))))


(let ((mw-log nil))
  (defun save-the-data (d) (push d mw-log))
  (defun reset-log () (setf mw-log nil))
  (defun get-mw-log () (reverse mw-log)))

(defun write-cw-log ()
  (let (field-list internal-time (handle (file-handle (logging *mw*))))
    (when handle
      (dolist (item (get-mw-log))
        (setq field-list (rest item) internal-time (first item))
        (write (rin (subject-info *mw*)) :stream handle)
        (write-char #\tab handle)
        (write (first (task-conditions)) :stream handle)
        (write-char #\tab handle)
        (write internal-time :stream handle)
        (dolist (field field-list)
          (write-char #\tab handle)
          (cond
           ((equal (type-of field) 'symbol)
            (if (capi:button-selected (write-symbols-as-strings (control-window *mw*)))
                (write (symbol-name field) :stream handle)
              (write field :stream handle)))
           ((equal (type-of field) 'string)
            (write-string field handle))
           (t
            (write field :stream handle))))
        (write-char #\newline handle)))))

;; Takes a list of values to log. Separates each with a tab, ends with a newline.
(defun log-info (field-list &optional &key (specify-time nil))
  (let ((internal-time (get-internal-real-time)))
    (if specify-time (setf internal-time specify-time))
    (if (capi:button-selected (delayed-file-io (control-window *mw*)))
        (save-the-data  (cons internal-time field-list))
      (write-the-data field-list internal-time)) 
    internal-time))

(defun write-the-data (field-list internal-time)
  (cond ((and (logging *mw*) (file-handle (logging *mw*))
           (open-stream-p (file-handle (logging *mw*)))
           (subject-info *mw*))
         (mp:with-lock ((resource-lock (logging *mw*)))
           (write (get-uid) :stream (file-handle (logging *mw*)))
           (write-char #\tab (file-handle (logging *mw*)))
           (write (first (task-conditions)) :stream (file-handle (logging *mw*)))
           (write-char #\tab (file-handle (logging *mw*)))
           (write internal-time :stream (file-handle (logging *mw*)))
           (dolist (field field-list)
             (write-char #\tab (file-handle (logging *mw*)))
             (cond
              ((equal (type-of field) 'symbol)
               (if (capi:button-selected (write-symbols-as-strings (control-window *mw*)))
                   (write (symbol-name field) :stream (file-handle (logging *mw*)))
                 (write field :stream (file-handle (logging *mw*)))))
              ((equal (type-of field) 'string)
               (write-string field (file-handle (logging *mw*))))
              (t
               (write field :stream (file-handle (logging *mw*))))))
           (write-char #\newline (file-handle (logging *mw*)))
        ;(force-output (file-handle (logging *mw*)))
        ))))

(defun log-header ()
  (let ((date-string nil))
    (multiple-value-bind
        (second minute hour day month year daylight zone other) 
        (get-decoded-time) 
      (declare (ignore second daylight zone other))
      (setf date-string (format nil "~d/~d/~d  ~d:~2d" month day year hour minute)))
    (log-info (list "CW-EVENT" "CW-VERSION" *version-string*))
    (log-info (list "CW-EVENT" "EXPERIMENT-NAME" (experiment-name *mw*)))
    (log-info (list "CW-EVENT" "SCREEN-RESOLUTION" (capi:screen-width (capi:convert-to-screen)) (capi:screen-height (capi:convert-to-screen))))
    (log-info (list "CW-EVENT" "DATE" date-string))
    (log-info (list "CW-EVENT" "Universal-time" (get-universal-time)))
    (log-info (list "CW-EVENT" "Unix-time" (unix-time)))
    (when (eql (control-mode *mw*) :human)
      (log-info (list "CW-EVENT" "ID" (get-uid)))
      (log-info (list "CW-EVENT" "EID" (aes8:rin->id (get-rin)))))
      ))



(defun logging-filename (&key (ftype "xls"))
  (case (control-mode *mw*)
    (:log-only
     (let* ((win (control-window *mw*))
            (dir (capi:title-pane-text (logging-folder win)))
            (fn (capi:text-input-pane-text (logging-fn win)))
            (ext-p (capi:item-selected (fn-date win))))
       (cond ((or (null dir) (< (length dir) 1))
              (capi:display-message "Invalid Director Name"))
             ((or (null fn) (< (length fn) 1))
              (capi:display-message "Invalid File Name"))
             (t
              (if ext-p
                  (setf fn (concatenate 'string fn (get-uid) ))
                (setf fn (concatenate 'string fn ".dat")))
              (merge-pathnames fn dir)))))
    ((:human :act-r)
     (let ((condition-string ""))
    
       (if (not (local-path *mw*))
           (progn
             (capi:display-message "Please select a folder to save logging data.~%To avoid this message in the future, add (define-logging-path PATHSTRING) to your task code.")
             (setf (local-path *mw*) (capi:prompt-for-directory "Please select a logging folder:"))))

       (dolist (task (task-list *mw*))
         (setf condition-string
               (string-append (task-condition task) condition-string))
         )
       
       (merge-pathnames 
          (make-pathname
           :name (format nil "~a-~2,'0D_~a-~a"
                         (experiment-name *mw*)
                         (experiment-version *mw*)
                         condition-string
                         (get-uid))
           :type ftype)
          (local-path *mw*))))))
