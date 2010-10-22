(push :response-pad *features*)

(let ((path (probe-file "/usr/lib/libResponsePad.dylib")))
  (if path
      (fli:register-module (probe-file "/usr/lib/libResponsePad.dylib") :connection-style :immediate)))

;(setf response-pad-id (response-pad-connect))
(fli:define-foreign-function  (response-pad-connect "connectResponsePad" :source)
    ()
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function  (response-pad-disconnect "disconnectResponsePad" :source)
    ((file-descriptor :int))
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function  (response-pad-read-byte "readByte" :source)
    ((file-descriptor :unsigned-int))
  :result-type :int
  :language :ansi-c)

(fli:define-foreign-function  (response-pad-reset-timer "resetBaseTimer" :source)
    ((file-descriptor :unsigned-int))
  :result-type :unsigned-byte
  :language :ansi-c)

;(if response-pad-id (mp:process-run-function "button box" nil #'response-pad-read-loop p))
(defstruct response-pad
  status
  response
  id)

(let ((response-pad (make-response-pad))
      (proc nil))
  (defun get-response-pad () response-pad)

  (defun get-response-pad-proc () proc)

  (defun connect-response-pad ()
    (setf (response-pad-id response-pad) (response-pad-connect))
    (if (eql -1 (response-pad-id response-pad) )
        (setf (response-pad-status response-pad) nil)
      (setf (response-pad-status response-pad) 'connected)))

  (defun enable-response-pad ()
    (when (response-pad-id response-pad) 
      (setf (response-pad-status response-pad) 'enabled)
      (setq proc (mp:process-run-function "button box" nil #'response-pad-read-loop ))))

  (defun disable-response-pad ()
    (setf (response-pad-status response-pad) 'disabled))

  (defun response-pad-status? ()
    (response-pad-status response-pad))
  
  (defun disconnect-response-pad ()
    (when (response-pad-status?)
      (setf (response-pad-status response-pad) nil)
      (response-pad-disconnect (response-pad-id response-pad))))

  (defun response-pad-read-loop ()
   (loop
     (cond ((eql (response-pad-status response-pad) 'enabled)
            (multiple-value-bind (button action timestamp) (response-pad-read-message (response-pad-id response-pad))
              (if (equal action 1) 
                (process-response button timestamp)) ;;;;;;;;specific to app
              )    
            (sleep 0.01))
           (t
            (return)))))
) ;end of let
;;;;;;;;;;;;;;;;;;;;;;
(defun clear-buffer (file-descriptor)
  (loop
   (multiple-value-bind (button action timestamp)
       (response-pad-read-message file-descriptor)
     (declare (ignore action timestamp))
     (if (not button) (return))))
  )

(defun response-pad-read-message (file-descriptor)
  (let ((k (response-pad-read-byte file-descriptor))
        (key-info -1)
        (action -1)
        (button -1)
        (t1 -1) (t2 -1) (t3 -1) (t4 -1)
        (timestamp 0))

    (cond
     ((= k 107)
      (loop
       (when (not (= key-info -1)) (return))
       (setf key-info (response-pad-read-byte file-descriptor)))
      (setf action (ldb (byte 1 4) key-info))
      (setf button (ldb (byte 3 5) key-info))

      (loop
       (when (not (= t1 -1)) (return))
       (setf t1 (response-pad-read-byte file-descriptor)))
      (loop
       (when (not (= t2 -1)) (return))
       (setf t2 (response-pad-read-byte file-descriptor)))
      (loop
       (when (not (= t3 -1)) (return))
       (setf t3 (response-pad-read-byte file-descriptor)))
      (loop
       (when (not (= t4 -1)) (return))
       (setf t4 (response-pad-read-byte file-descriptor)))
      
      (setf timestamp (dpb t1 (byte 8 0) timestamp))
      (setf timestamp (dpb t2 (byte 8 8) timestamp))
      (setf timestamp (dpb t3 (byte 8 16) timestamp))
      (setf timestamp (dpb t4 (byte 8 24) timestamp))
      (values button action timestamp))
     ;((not (= k -1))
     ; (format t "*** ~a~%" k))
     (t
      nil))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Response Pad Demo

(defun response-pad-demo ()
  (let ((fd (response-pad-connect))
        (pane (make-instance 'capi:output-pane
                             :visible-min-width 340
                             :visible-min-height 180
                             :visible-max-width 340
                             :visible-max-height 180
                             :background :gray))
        (visible nil)
        (b1 0) (b2 0) (b3 0) (b4 0)
        (b5 0) (b6 0) (b7 0) (b0 0))
    (cond
     ((= fd -1)
      (capi:display-message "Could not connect to Response Pad."))
     (t
      (capi:contain pane)
      (setf visible (capi:interface-visible-p pane))
      (response-pad-reset-timer fd)
      
      (loop
       (setf visible (capi:interface-visible-p pane))
       (when (not visible) (return))
       (capi:process-pending-messages nil)
       
 
       (loop
        (multiple-value-bind (button action timestamp) (response-pad-read-message fd)
          (if (not button) (return))
          (case button
            (1 (setf b1 action))
            (2 (setf b2 action))
            (3 (Setf b3 action))
            (4 (setf b4 action))
            (5 (setf b5 action))
            (6 (setf b6 action))
            (7 (setf b7 action))
            (0 (setf b0 action)))
          (if button
              (format t "~a ~a ~a~%" button action timestamp))))
       
       (capi:apply-in-pane-process
        pane 'response-pad-demo-draw-pane pane b1 b2 b3 b4 b5 b6 b7 b0)

       (sleep 0.01)

       )
      ))
    (response-pad-disconnect fd)
    )
  )

(defun response-pad-demo-draw-pane (pane b1 b2 b3 b4 b5 b6 b7 b0)
  (if (= b5 1)
      (gp:draw-rectangle pane 20 40 40 20 :filled t :foreground :black)
    (gp:draw-rectangle pane 20 40 40 20 :filled t :foreground :white))
  (if (= b7 1)
      (gp:draw-rectangle pane 20 80 40 20 :filled t :foreground :yellow)
    (gp:draw-rectangle pane 20 80 40 20 :filled t :foreground :white))
  (if (= b1 1)
      (gp:draw-rectangle pane 80 40 80 40 :filled t :foreground :black)
    (gp:draw-rectangle pane 80 40 80 40 :filled t :foreground :white))
  (if (= b3 1)
      (gp:draw-rectangle pane 80 100 80 40 :filled t :foreground :blue)
    (gp:draw-rectangle pane 80 100 80 40 :filled t :foreground :white))
  (if (= b4 1)
      (gp:draw-rectangle pane 180 100 80 40 :filled t :foreground :blue)
    (gp:draw-rectangle pane 180 100 80 40 :filled t :foreground :white))
  (if (= b2 1)
      (gp:draw-rectangle pane 180 40 80 40 :filled t :foreground :black)
    (gp:draw-rectangle pane 180 40 80 40 :filled t :foreground :white))
  (if (= b6 1)
      (gp:draw-rectangle pane 280 40 40 20 :filled t :foreground :black)
    (gp:draw-rectangle pane 280 40 40 20 :filled t :foreground :white))
  (if (= b0 1)
      (gp:draw-rectangle pane 280 80 40 20 :filled t :foreground :red)
    (gp:draw-rectangle pane 280 80 40 20 :filled t :foreground :white))


  (capi:process-pending-messages nil)
  )

