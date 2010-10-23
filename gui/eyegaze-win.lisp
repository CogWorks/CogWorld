
(capi:define-interface eyegaze-win ()
  ()
  (:panes
   (output capi:output-pane
           :visible-min-width 30
           :visible-max-width 30
           :visible-min-height 30
           :visible-max-height 30
           :accessor output
           :background (color:make-rgb 0.0 0.0 0.0 0.0)
           :input-model '(((:button-1 :press) close-eyegaze-win))
           :display-callback 'draw-eyegaze-pane))
  (:layouts
   (main capi:simple-layout '(output)))
  (:default-initargs
   :window-styles '(:borderless :internal-borderless :always-on-top)
   :background :red
   )
  )

(defun close-eyegaze-win (self x y)
  (declare (ignore x y))
  (capi:apply-in-pane-process
   self 'capi:destroy (capi:element-interface self)))

(defun draw-eyegaze-pane (self x y width height)
  (gp:draw-circle self (+ x (/ width 2)) (+ y (/ height 2)) (/ width 3)
                  :graphics-args '(:thickness 5
                                   :foreground :red)))

(defun create-eyegaze-window ()
  (if (eyegaze-window *cw*)
      (capi:apply-in-pane-process
       (eyegaze-window *cw*) 'capi:destroy (eyegaze-window *cw*)))
  (setf (eyegaze-window *cw*) (make-instance 'eyegaze-win))
  (capi:display (eyegaze-window *cw*)))

(defun move-eyegaze-window (x y)
  (let ((win (eyegaze-window *cw*)))
    (capi:apply-in-pane-process
     (output win)
     #'(lambda ()
         (capi:set-top-level-interface-geometry
          win :x (- x 15) :y (- y 15))
         (capi:raise-interface win)))))

(defun hide-eyegaze-window ()
  (let ((win (eyegaze-window *cw*))
        (pane nil))
    (if win
        (progn (setf pane (output win))
          (capi:apply-in-pane-process
           pane 'capi:hide-interface pane nil)))))

(defun show-eyegaze-window ()
  (let ((win (eyegaze-window *cw*))
        (pane nil))
    (if win
        (progn (setf pane (output win))
          (capi:apply-in-pane-process
           pane 'capi:show-interface pane)))))
