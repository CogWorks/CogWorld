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

(defun delete-nth (sequence n)
  (delete-if (constantly t) sequence :start n :count 1))

(defmacro conv-to-list (x)
  `(if ,x (ignore-errors (read-from-string (concatenate 'string "(" (string-trim '(#\Space #\. #\?) ,x) ")")))))

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
