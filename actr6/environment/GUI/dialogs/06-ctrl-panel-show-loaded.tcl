label .control_panel.current_load -text "Current Model" -font label_font

label .control_panel.load_model_name \
      -font text_font -textvariable .control_panel.load_model_name.var

pack .control_panel.current_load
pack .control_panel.load_model_name

set .control_panel.load_model_name.var "No Model"

send_environment_cmd \
  "create simple-handler .control_panel.load_model_name \
    .control_panel.load_model_name.var \
    (lambda (x) (if (symbolp x) (if x x \"No Model\") (aif (current-model) it \"No Model\"))) (create delete)"

bind .control_panel.load_model_name <Destroy> {
  remove_handler %W
}

