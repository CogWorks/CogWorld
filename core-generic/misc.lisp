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
