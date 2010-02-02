proc select_visicon {} {

  if {[winfo exists .visicon] == 1} {
    wm deiconify .visicon
    raise .visicon
  } else {
    # make it now

    toplevel .visicon
    wm withdraw .visicon
    wm title .visicon "Visicon"

    wm geometry .visicon [get_configuration .visicon]

    frame .visicon.frame -borderwidth 0  
    
    text .visicon.frame.text -font text_font \
         -yscrollcommand ".visicon.frame.scrl set" \
         -state disabled
          
    scrollbar .visicon.frame.scrl \
              -command ".visicon.frame.text yview"

    send_environment_cmd \
      "create text-output-handler .visicon.frame.text \
         .visicon.frame.text (lambda (x) (declare (ignore x)) (print-visicon)) (post)"

    bind .visicon.frame.text <Destroy> {
      remove_handler .visicon.frame.text
    }

    # Make the window useable for copy operations on Windows
  
    bind .visicon.frame.text <1> {focus %W}
  
    pack .visicon.frame.scrl -side right -fill y 
    pack .visicon.frame.text -side left -expand 1 -fill both
  
    place .visicon.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0 

    # now show the window 

    wm deiconify .visicon
  }
}

button .control_panel.visicon_button \
       -command {select_visicon} -text "Visicon" -font button_font

pack .control_panel.visicon_button

