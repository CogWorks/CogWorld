frame .control_panel.run_frame -borderwidth 0 -width 130

button .control_panel.run_frame.run -text "Run" -font button_font -command {
  send_environment_cmd \
    "update [get_handler_name .control_panel.run_frame.run] \
       (lambda (x) \
          (declare (ignore x)) \
            (unwind-protect \
             (if *running-actr* \
                (model-warning \"Run button has no effect because model still running when pressed.\") \
               (progn (setf *running-actr* t) \
                     (run $run_button_time) \
               )) \
             (setf *running-actr* nil)))"
}                    

entry .control_panel.run_frame.run_time -width 6 -font text_font \
      -textvariable run_button_time -justify right

.control_panel.run_frame.run_time insert 0 "10.0"

pack .control_panel.run_frame.run -side left
pack .control_panel.run_frame.run_time -side right

pack .control_panel.run_frame

send_environment_cmd \
  "create simple-handler .control_panel.run_frame.run ignore_returns \
    (lambda (x) (declare (ignore x))) ()"

bind .control_panel.run_frame.run <Destroy> {
  remove_handler %W
}

