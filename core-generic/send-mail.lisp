;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Filename     : SendMaill.lisp;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Author       : Evan Patton, Mike Schoelles;; Copyright    : (C) 2009 CogWorks Laboratory;; Address      : Cognitive Science Department;;              : Rennselaer Polytechnic Institute;;              : Troy, NY 12180;;              : schoem@rpi.edu;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(defparameter *SMTP-SERVER* "mail.rpi.edu")(defparameter *SMTP-PORT* 25)(defun url-escape (str)  (let ((result "")        (c nil))    (dotimes (i (length str) result)      (setf c (elt str i))      (cond ((and (char>= c #\0) (char<= c #\9))             (setf result (format nil "~A~C" result c)))            ((and (char>= c #\A) (char<= c #\Z))             (setf result (format nil "~A~C" result c)))            ((and (char>= c #\a) (char<= c #\z))             (setf result (format nil "~A~C" result c)))            (t             (setf result (format nil "~A%~2,'0X" result (char-code c))))))))(defun send-mail-smtp (sender addresses subject body &key cc echo)  (let ((recipients nil)        (stream nil))    (cond ((listp addresses)           (setf recipients (copy-list addresses)))          ((stringp addresses)           (setf recipients (list addresses)))          (t           (error "Invalid value supplied for addresses")))    (cond ((and cc (listp cc))           (setf recipients (append recipients (copy-list cc))))          ((and cc (stringp cc))           (setf recipients (append recipients (list cc))))          (cc           (error "Invalid value supplied for cc")))    (setf stream (comm:open-tcp-stream *SMTP-SERVER* *SMTP-PORT* :direction :io))    (if (not stream) (error "Unable to connect to SMTP server."))    (format echo "~A~&" (read-line stream))    (format echo "HELO cogworks.cogsci.rpi.edu~%")    (format stream "HELO cogworks.cogsci.rpi.edu~%")    (finish-output stream)    (format echo "~A~&" (read-line stream))    (format echo "MAIL FROM:<~A>~%" sender)    (format stream "MAIL FROM:<~A>~%" sender)    (finish-output stream)    (dolist (r recipients)      (format echo "~A~&" (read-line stream))      (format echo "RCPT TO:<~A>~%" r)      (format stream "RCPT TO:<~A>~%" r)      (finish-output stream))    (format echo "~A~&" (read-line stream))    (format echo "DATA~%")    (format stream "DATA~%")    (finish-output stream)    (format echo "~A~&" (read-line stream))    (format echo "From: ~A~%" sender)    (format stream "From: ~A~%" sender)    (cond ((listp addresses)           (dolist (addr addresses)             (format echo "To: ~A~%" addr)             (format stream "To: ~A~%" addr)))          ((stringp addresses)           (format echo "To: ~A~%" addresses)           (format stream "To: ~A~%" addresses)))    (cond ((and cc (listp cc))           (dolist (addr cc)             (format echo "Cc: ~A~%" addr)             (format stream "Cc: ~A~%" addr)))          ((stringp cc)           (format echo "Cc: ~A~%" cc)           (format stream "Cc: ~A~%" cc)))    (format echo "Subject: ~A~%~%" subject)    (format stream "Subject: ~A~%~%" subject)    (format echo "~A" body)    (format stream "~A" body)    (format echo "~%.~%")    (format stream "~%.~%")    (finish-output stream)    (format echo "~A~&" (read-line stream))    (format echo "QUIT~%")    (format stream "QUIT~%")    (finish-output stream)    (format echo "~A~&" (read-line stream))    (close stream)))(defun get-system-email-address ()  "grayw@rpi.edu ")(defun conv-to-list (x)  (cond (x         (read-from-string (concatenate 'string "(" (string-trim '(#\Space #\. #\?) x) ")")))))(defun extract-id-from-filename(fn)  (let ((pos (position #\. fn)))    (subseq fn (- pos (length "yrmndy_hrmnsc")) pos)))(let ((subj-id-accum nil))(defun mail-history-files (exp-name  &key (from (get-system-email-address)) to path)  (let* ((files (directory (capi:prompt-for-directory nil :pathname path)))         (name (if (stringp exp-name) exp-name (write-to-string exp-name)))         (len  (length name ) )         (body ""))    (if (null to)        (setq to (capi:prompt-for-string "Enter address or coma separated address list:")))     (when to           (dolist (f files)         (setq f (namestring f))         (when (and (> (length f) (+ len 21)) ;"id.history"                   (equalp (subseq (file-namestring f) 0 len) exp-name)                   (equal (subseq (reverse f) 0 (length "history"))  (reverse "history")))           (with-open-file (fs f :direction :input)              (read-line fs)            (let* ((ln (ignore-errors (read-line fs)))                   (lst (if ln (read-from-string (concatenate 'string "(" (string-trim '(#\Space #\. #\?) ln) ")"))))                   (rin (sixth lst)))              (when ln                (setq body (concatenate 'string body ln (format nil "~%")))                     (push (list (aes8:rin->id rin) (extract-id-from-filename (file-namestring f))) subj-id-accum))))))       (if (plusp (length body))          (send-mail-smtp from to name body)))))(defun send-ids-to-server () (dolist (ids (reverse subj-id-accum))   (destructuring-bind (pid sid) ids     (add-ids pid sid)))))(defun onelinepage (server port page data)  (let ((answer ""))    (with-open-stream (http (comm:open-tcp-stream                              server port))      (format http "POST /~a HTTP/1.1~C~CHost: ~a~C~CContent-type: application/x-www-form-urlencoded~C~CContent-length: ~a~C~CConnection: close~C~C~C~C~a~C~C"              page               (code-char 13) (code-char 10)              server              (code-char 13) (code-char 10)              (code-char 13) (code-char 10)              (length data)              (code-char 13) (code-char 10)              (code-char 13) (code-char 10)              (code-char 13) (code-char 10)              data              (code-char 13) (code-char 10))      (force-output http)      ;(write-string "Waiting to reply...")      (loop for ch = (read-char-no-hang http nil :eof)            until ch            do ;(write-char #\.)            (sleep 0.25)            finally (unless (eq ch :eof)                      (unread-char ch http)))       (terpri)      (loop for line = (read-line http nil nil)            while line            do (if (find #\. line) (setq answer line))))    answer))(defun get-sids (pid)  (onelinepage "cog3.cogsci.rpi.edu" 8080 "ids.cgi"               (format nil "action=get&pid=~a&usr=cwl&pwd=IDSdbsimon" pid)))(defun get-pids (sid)  (onelinepage "cog3.cogsci.rpi.edu" 8080 "ids.cgi"               (format nil "action=get&sid=~a&usr=cwl&pwd=IDSdbsimon" sid)))(defun add-ids (pid sid)  (onelinepage "cog3.cogsci.rpi.edu" 8080 "ids.cgi"                (format nil "action=put&pid=~a&sid=~a&usr=cwl&pwd=IDSdbsimon" pid sid)))(defun finish-experiment (exp-name to path)  (mail-history-files exp-name :to to :path path)  (send-ids-to-server))    