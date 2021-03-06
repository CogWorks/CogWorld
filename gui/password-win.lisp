(capi:define-interface password-dialog ()
  ((password :initform nil :initarg :password :accessor password))
  (:panes
   (message-pane capi:display-pane
                 :text ""
                 :background :transparent
                 :accessor message-pane
                 :font (gp:make-font-description :size 14))
   (input-pane capi:password-pane
               :text ""
               :accessor input-pane)
   (ok-button capi:push-button
              :text "OK"
              :callback 'password-done
              :default-p t)
   )
  (:layouts
   (main capi:column-layout '(message-pane input-pane ok-button) :adjust :center)
   )
  (:default-initargs
   :background :transparent
   )
  )

(defun password-done (data interface)
  (declare (ignore data))
  (if (equal (capi:text-input-pane-text (input-pane interface)) (password interface))
      (capi:exit-dialog nil))
  )

(defun prompt-for-password (&key (message "Please enter password:") (password ""))
  (let ((interface (make-instance 'password-dialog)))
    (setf (capi:display-pane-text (message-pane interface)) message)
    (setf (password interface) password)
    (capi:display-dialog interface)
    )
  )
