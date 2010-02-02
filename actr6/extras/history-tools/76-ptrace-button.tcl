
global ptrace_array

proc select_ptrace {} {

    set win [toplevel [new_variable_name .ptrace]]

    global $win.scale
    global ptrace_array

    set ptrace_array($win,0) 0
    set ptrace_array($win,1) 0

    wm withdraw $win
    wm title $win "Production Selection History"

    wm geometry $win [get_configuration .ptrace $win]


    frame $win.frame -borderwidth 0  
    
    canvas $win.frame.canvas  \
         -xscrollcommand "$win.frame.scrlx set" \
         -yscrollcommand "$win.frame.scrly set" \
         -width 900 -height 300 -scrollregion {0 0 900 300} -bg white
   
    canvas $win.frame.canvas1  \
         -yscrollcommand "$win.frame.scrly set" \
         -width 150 -height 300 -scrollregion {0 0 150 300} -bg white
   
       
    scrollbar $win.frame.scrlx \
              -command "$win.frame.canvas xview" -orient horizontal

    scrollbar $win.frame.scrly \
              -command "scroll_ptraces_canvas $win" -orient vertical


    set $win.scale 1.0
          
    send_environment_cmd \
      "create list-handler $win.frame.canvas $win.return \
         (lambda (x) (declare (ignore x)) (no-output (sgp :save-p-history t)) nil) (reset)"


    bind $win.frame.canvas <Destroy> "remove_handler $win.frame.canvas"


    button $win.update -command "draw_p_trace $win" -text "Get history" -font button_font

    label $win.text -font text_font  -textvariable $win.textvar
  
    set $win.textvar ""

    label $win.notes -font text_font  -textvariable $win.notesvar -anchor nw
  
    set $win.notesvar ""

    button $win.grid -command "p_trace_grid $win" -text "Grid" -font button_font

    button $win.zoom_in -command "p_trace_zoom_in $win" -text "+" -font button_font

    button $win.zoom_out -command  "p_trace_zoom_out $win" -text "-" -font button_font


    global $win.scale
    set $win.scale 1.0

    global $win.grid_state
    set $win.grid_state 1

    global $win.c_height
    global $win.c_width

    pack $win.frame.scrlx -side bottom -fill x
    pack $win.frame.scrly -side right -fill y
    pack $win.frame.canvas1 -side left -fill y
    pack $win.frame.canvas -side left -fill both
 
    place $win.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0 -height -40
    
    place $win.notes -x 120 -rely 1.0 -y -39 -relwidth 1.0 -height 39
    place $win.text -x 0 -rely 1.0 -y -20 -width 75 -height 20

    place $win.update -x 0 -rely 1.0 -y -40 -width 75 -height 20
    place $win.grid -x 75 -rely 1.0 -y -40 -width 40 -height 20
    place $win.zoom_in -x 75 -rely 1.0 -y -20 -width 20 -height 20
    place $win.zoom_out -x 95 -rely 1.0 -y -20 -width 20 -height 20
    
    wm deiconify $win
} 


proc scroll_ptraces_canvas {win args} {

   set ignore ""
   eval [append ignore $win.frame.canvas " " yview " " $args]
   set ignore ""
   eval [append ignore $win.frame.canvas1 " " yview " " $args]
}

proc draw_p_trace {win} {

  global $win.return

  $win.frame.canvas delete all 
  $win.frame.canvas1 delete all

  upvar $win.textvar display
                
  set display "Busy"

    global $win.scale
    global $win.grid_state
    global $win.c_height
    global $win.c_width


   upvar $win.scale scale
   upvar $win.grid_state grid
   upvar $win.c_height g_c_height
   upvar $win.c_width g_c_width

  set c_height 0
  set c_width 0
  set t_height 0
  set t_width 0
  set n_width 0
  set x_display 0
   
  $win.update configure -state disabled
  $win.grid configure -state disabled
  $win.zoom_in configure -state disabled
  $win.zoom_out configure -state disabled


  set done 0

  while {$done == 0} {
             
  set $win.return ""
                  
  send_environment_cmd "update [get_handler_name $win.frame.canvas] production-history-graph-data"

  wait_for_non_null $win.return

  upvar $win.return result

  foreach x $result {
    switch [lindex $x 0] {
      labels { 
        set y $c_height
        
        foreach p [lrange $x 1 end] {
          set box_name [new_variable_name box]
 
          $win.frame.canvas1 create text 5 $y -anchor nw -font text_font -text $p -tag $box_name
         # $win.frame.canvas create line 0 [lindex $x 2] $width [lindex $x 2] -width 1 -f gray
          $win.frame.canvas1 create line 0 $y $n_width $y -width 1 -f gray

          $win.frame.canvas1 bind $box_name <ButtonPress> "p_history_p_view $p"
          
          incr y $c_height
        }
      }

      colors {
          set colors [lrange $x 1 end]
      }

      reasons {
          set reasons [lrange $x 1 end]
      }
      
      size { 
       set t_height [lindex $x 1]
       set c_height [lindex $x 2]
       set n_width [lindex $x 3]
       set c_width [lindex $x 4]
       set t_width [lindex $x 5]

       $win.frame.canvas configure -width $t_width -height $t_height
       $win.frame.canvas1 configure -width $n_width -height $t_height
       $win.frame.canvas1 configure -scrollregion "0 0 $n_width $t_height"
       $win.frame.canvas configure -scrollregion "0 0 $t_width $t_height"
      }

      done {
       set done 1
      }

      column {
        
        $win.frame.canvas create text [expr $x_display + ($c_width / 2)] 0 -anchor n -font graphic_trace_font -text [lindex $x 1]

        set y $c_height

        foreach {index value} [lrange $x 2 end] {

          set box_name [new_variable_name box]
 
          $win.frame.canvas create rectangle $x_display $y [expr $x_display + $c_width] [expr $y + $c_height] -width 0 -fill [lindex $colors $index] -tag $box_name
  
          switch $index {
              0 {
               $win.frame.canvas bind $box_name <Enter> "set $win.notesvar \"Utility: $value\""
              }
              1 {
                $win.frame.canvas bind $box_name <Enter> "set $win.notesvar \"Utility: $value\""
              }
              2 {
                $win.frame.canvas bind $box_name <Enter> "set $win.notesvar \"Whynot: [lindex $reasons $value]\""
              }
          }

          $win.frame.canvas bind $box_name <Leave>  "set $win.notesvar \"\""
          incr y $c_height
        }
        incr x_display $c_width
      }
    }
  }
  }

  send_environment_cmd "update [get_handler_name $win.frame.canvas]  \
         (lambda (x) (declare (ignore x)) (no-output (sgp :save-p-history t)) nil)"


  for {set x 0} {$x < $t_width} {incr x $c_width} {
     $win.frame.canvas create line $x 0 $x $t_height -width 1 -f black -tag [list grid grid_vert]
  }

  for {set y $c_height} {$y < $t_height} {incr y $c_height} {
     $win.frame.canvas create line 0 $y $t_width $y -width 1 -f black -tag grid
  }

  set scale 1.0
  set grid black
  set g_c_height $c_height
  set g_c_width $c_width
  
  $win.update configure -state normal
  $win.grid configure -state normal
  $win.zoom_in configure -state normal
  $win.zoom_out configure -state normal

  set display "Done"
}

proc p_trace_zoom_out {win} {

   global $win.scale
   upvar $win.scale scale

   set scale [expr .5 * $scale]
 
   $win.frame.canvas scale all 0 0 0.5 1.0
   $win.frame.canvas configure -width [expr .5 * [$win.frame.canvas cget -width]]
   $win.frame.canvas configure -scrollregion "0 0 [$win.frame.canvas cget -width] [$win.frame.canvas cget -height]"
  

#   foreach x [$win.frame.canvas find withtag trace_text] {
#      $win.frame.canvas itemconfigure $x -width [expr .5 * [$win.frame.canvas itemcget $x -width]]
#   }
}

proc p_trace_zoom_in {win} {

   global $win.scale
   upvar $win.scale scale

   if {$scale < 16} {
      set scale [expr 2 * $scale]

      $win.frame.canvas scale all 0 0 2.0 1.0
   $win.frame.canvas configure -width [expr 2.0 * [$win.frame.canvas cget -width]]
   $win.frame.canvas configure -scrollregion "0 0 [$win.frame.canvas cget -width] [$win.frame.canvas cget -height]"
      
#      foreach x [$win.frame.canvas find withtag trace_text] {
#         $win.frame.canvas itemconfigure $x -width [expr 2 * [$win.frame.canvas itemcget $x -width]]
#      }
   }
}


proc p_trace_grid {win} {

   global $win.grid_state
   upvar $win.grid_state grid

   if {$grid == ""} {
    $win.frame.canvas itemconfigure grid -f black
    set grid black
   } elseif {$grid == "vert"} {
    $win.frame.canvas itemconfigure grid -f ""
    set grid ""
   } else {
    $win.frame.canvas itemconfigure grid_vert -f ""
    set grid vert
   }

#   foreach x [$win.frame.canvas find withtag trace_text] {
#      $win.frame.canvas itemconfigure $x -width [expr .5 * [$win.frame.canvas itemcget $x -width]]
#   }
}

proc p_history_p_view {prod} {
 
    set win [make_procedural_viewer]

    set box "$win.list_frame.list_box"

    global  $win.list_frame.list_box.var
    wait_for_non_null $win.list_frame.list_box.var

    set index [lsearch -exact [$box get 0 end] $prod] 

    $box selection set $index

    event generate $box <<ListboxSelect>>
}


button .control_panel.ptrace_button \
       -command {select_ptrace} -text "Production History" -font button_font

pack .control_panel.ptrace_button

