proc select_audicon {} {

  if {[winfo exists .audicon] == 1} {
    wm deiconify .audicon
    raise .visicon
  } else {
    # make it now

    toplevel .audicon
    wm withdraw .audicon
    wm title .audicon "Audicon"

    wm geometry .audicon [get_configuration .audicon]

    frame .audicon.frame -borderwidth 0  
    
    text .audicon.frame.text -font text_font \
         -yscrollcommand ".audicon.frame.scrl set" \
         -state disabled
          
    scrollbar .audicon.frame.scrl \
              -command ".audicon.frame.text yview"

    send_environment_cmd \
      "create text-output-handler .audicon.frame.text \
         .audicon.frame.text (lambda (x) (declare (ignore x)) (print-audicon)) (post)"

    bind .audicon.frame.text <Destroy> {
      remove_handler .audicon.frame.text
    }

    # Make the window useable for copy operations on Windows
  
    bind .audicon.frame.text <1> {focus %W}
  
    pack .audicon.frame.scrl -side right -fill y 
    pack .audicon.frame.text -side left -expand 1 -fill both
  
    place .audicon.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0 

    # now show the window 

    wm deiconify .audicon
  }
}

button .control_panel.audicon_button \
       -command {select_audicon} -text "Audicon" -font button_font

pack .control_panel.audicon_button

