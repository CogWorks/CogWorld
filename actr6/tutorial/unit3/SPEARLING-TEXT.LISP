(capi:define-interface sperling ()
 ()
 (:panes
  (disp capi:display-pane :text '("A           B           C           D" 
                                  "E           F           G           H" "I            J            K            L")))
 (:default-initargs
  :best-width 200
  :best-height 200))

(defun sperling-test (tm)
  (let ((win (capi:display (make-instance 'sperling))))
    (sleep tm)
    (capi:destroy win)))