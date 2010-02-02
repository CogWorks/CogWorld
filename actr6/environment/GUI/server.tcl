# 
# This is the main environment file.
# It contains the procedures and variables for the network
# communication, sources all of the other files, creates the
# control panel window as .control_panel, and opens the listening socket.

# This variable is used to conditionalize code that
# operates differently when the environment is being
# built into a standalone application.

global standalone_mode

# Use 0 for not standalne
# Use 1 for Windows standalone
# Use 2 for Mac standalone

set standalone_mode 0

# This variable is an array that will be used to hold the
# table of Lisp handler names indexed by Tcl window name.
# These get entered by the register command coming from Lisp.

global handler_names


# valid_handler_name
# Given a tcl window (w) return true if there is 
# currently an entry in the table of that window's 
# Lisp handler and 0 if there is not.

proc valid_handler_name {w} {
  global handler_names
  return [llength [array names handler_names $w]] 
}

# get_handler_name
# Given a tcl window (w) return the name of the Lisp handler
# associated with that window.  There is no check to see if
# that window has a handler - that must be done prior to calling
# this procedure.

proc get_handler_name {w} {
  global handler_names
  return $handler_names($w)
}

# set_handler_name 
# Given a tcl window (w) and the name of a Lisp handler (h)
# set the handler entry of w to be h in the global lookup table.

proc set_handler_name {w h} {
  global handler_names
  set handler_names($w) $h
}


# This variable and the next procedure are used to implement
# basically a gentemp in Tcl.  The reason I need it is because every 
# dialog that comunicates with Lisp needs a unique name and 
# probably a variable to be set to return values.  This allows
# me to easily create multiple instances of the "standard" 
# environment dialogs.

global variable_count_array


global time_to_exit 
set time_to_exit 1

# new_variable_name
# Given any variable name prefix (prefix) return that 
# prefix with the next number appended to it.  Doesn't 
# gurantee uniqueness though because if you use name#
# variables elswhere this could end up generating that
# same name.
 
proc new_variable_name {prefix} {
  global variable_count_array
  if [llength [array names variable_count_array $prefix]] {
    return "$prefix[incr variable_count_array($prefix)]" 
  } else {
    return "$prefix[set variable_count_array($prefix) 1]"
  }
}

# record the Environment's directory
global top_dir
global tcl_env_dir

set tcl_env_dir [pwd]
cd ..
if {$standalone_mode == 0} {cd ..}

set top_dir [pwd]

if {$standalone_mode == 0} {cd "environment"}

cd "GUI"


# To make sure everything is the "same size" on all systems
# I need to set a consistent scaling - most important for the
# Environment display of virtual windows otherwise the fixation
# cursor isn't in the 'right' place ...

tk scaling 1.0


# Define some general file handling procedures.
# Taken pretty much right from the Tcl/Tk book I've got

proc read_file { filename } {
  set data ""
  if {[file readable $filename]} {
    set fileid [open $filename "r"]
    set data [read $fileid]
    close $fileid
  }

  return $data
}

proc save_text {textwidget filename} {
  set data [$textwidget get 1.0 {end -1c}]
  write_data $data $filename
}

proc write_data {data filename} {
  set fileid [open $filename "w"]
  puts -nonewline $fileid $data
  # I know I shouldn't have to flush before closing, but I've
  # got a weird synchronization problem that seems to be caused
  # by just this type of thing
  flush $fileid
  close $fileid
}

proc append_data {data filename} {
  set fileid [open $filename "a"]
  puts -nonewline $fileid $data
  # I know I shouldn't have to flush before closing, but I've
  # got a weird synchronization problem that seems to be caused
  # by just this type of thing
  flush $fileid
  close $fileid
}


# Start by hiding .

wm withdraw .

# source all the init folder .tcl files.
# They're sourced in sorted order, so prepending a # to the
# beginning of the name helps enforce an order on them.

cd init

foreach f [lsort [glob -nocomplain *.tcl]] {source $f}

cd ..

# Create the Control Panel here...

toplevel .control_panel

wm title .control_panel "Control Panel"

wm geometry .control_panel [get_configuration .control_panel]

# for now just have a label to report errors.

label .control_panel.status -textvariable global_status -text "" -borderwidth 1
pack .control_panel.status

# report_status
# Display the provided string (s) on the status label of the control Panel.

#proc report_status {s} {
##  global global_status
#  set global_status "$global_status $s"
#}


# To avoid overwriting on big reports, instead
# dump it to an error log

file delete [file join $tcl_env_dir "error.log"]

proc report_status {s} {
  global global_status
  global tcl_env_dir
  set global_stats "ERROR Logged"
  append_data $s [file join $tcl_env_dir "error.log"]
}




# Will need to expand this later, but for now make sure
# that the socket gets closed when the program ends 

bind .control_panel <Destroy> {
  global time_to_exit

  save_window_positions

  catch {
    global environment_socket
    global connection_socket
    if {$environment_socket != ""} {
      close $environment_socket
    }
  }
  catch {
    if {$connection_socket != ""} {
      close $connection_socket
    }
  }
  
  set_return_result $time_to_exit
}

# For debugging I need access to the console window, so 
# here's how to get it ->  shift-control-L

bind .control_panel <Shift-Control-Key-L> {console show}

wm protocol .control_panel WM_DELETE_WINDOW {
  shut_it_down 0
}

proc shut_it_down {now} {
  global current_open_model 
  global local_connection
  global standalone_mode

  set destroy 1
         
  # there are other ways to test for a closed socket, but trying to
  # send a command seems easy and robust enough...

  if {$now == 0 && $local_connection == 1 && \
      [check_connection] == 0} {
    if $standalone_mode {
      tk_messageBox -icon warning -title "Open connection" \
                    -message "To quit the environment close the Listener window." -type ok
    } else {
      tk_messageBox -icon warning -title "Open connection" \
                    -message "You must close the connection from the Lisp side first." -type ok
    }

    set destroy 0
  } elseif {$now == 0} { # I know there should be a better order here, but
                         # but this simple setup isn't too bad for now
    #tk_messageBox -icon warning -title "Closing Environment" \
    #              -message "ACT-R Environment exiting" -type ok
    exit
    
  } elseif {$current_open_model != ""} {
    set answer [tk_messageBox -icon warning -title "Model still open" \
                  -message "There is a model open.  Should the environment \
                            close the model before quitting (all unsaved\
                            changes will be lost if it is not closed)?" \
                  -type yesno]

    switch -- $answer {
      yes {
        close_model
      }
    }
  }

  if $destroy {destroy .control_panel}
}

proc check_connection {} {
  global environment_socket
  return [catch {puts $environment_socket "(k-a)<end>"}] 
}

# These are the procedures for handling the communications.
# There is no handshaking on the commands, because it uses TCP
# sockets which are supposed to do that stuff for you...

# accept_socket_commands
# This procdure gets called everytime there is something to read
# on the socket connected to ACT-R.  The parameter passed is the
# socket.
# It reads lines from the socket until a whole command has been read
# (there is an explicit <end> marker now sent to handle new-lines 
# properly).  Then it passes that command to handle_socket_command.

# Just ignoring the case where there "isn't" something for now...

set last_command ""

proc accept_socket_commands {sock} {
  global last_command
  if {-1 != [gets $sock line]} {
    if [regexp {(.*)<end>$} $line dummy cmd] {
      handle_socket_command "$last_command$cmd"
      set last_command ""
    } else { 
      set last_command "$last_command$line\n"
    }
  }
}


# handle_socket_command
# This procedure takes one parameter which is a string that represents
# a command sent from Lisp.
# The commands that can be accepted must be of the form:
# {command} {arg1} {rest}
# command must be either update or register 
# arg1 and rest depend on the command, but must be non-empty.
# "bad" commands get reported in the status of the control panel.
    
proc handle_socket_command {data} {
  global time_to_exit

  if [regexp -nocase {^([a-z]+)([ ]+)([^ ]+)([ ]+)(.+)$} \
             $data dummy cmd blank arg1 blank rest] {
    switch $cmd {
      update {
        if [regexp -nocase \
                   {^([^ ]+)([ ]+)(.+)$} $rest dummy target blank value] {
          switch $arg1 {
            simple {
              global $target
              if {$value == "EMPTY_ENV_STRING"} {
                set $target ""
              } else {
                set $target $value
              }
            }
            special {
              global $target
                set $target $value
            }
            simple_funcall {
             
              set eval_cmd ""
              if {$value == "EMPTY_ENV_STRING"} {
                eval [append eval_cmd $target " " "\"\""]
              } else {
                eval [append eval_cmd $target " " $value]
              }
            }
            text {
              catch {
                if [valid_handler_name $target] {
                  $target configure -state normal
                  $target delete 1.0 end
                  if {$value != "EMPTY_ENV_STRING"} {
                    $target insert 1.0 $value
                    $target configure -state disabled
                  }
                }
              }
            }
            simpletext {
              catch {
                if [valid_handler_name $target] {
                  # $target configure -state normal
                  if {$value != "EMPTY_ENV_STRING"} {
                    $target insert end $value
                    $target yview -pickplace end
                  }
                  # $target configure -state disabled
                }
              }
            }      
            list_box {
              catch {
                if [valid_handler_name $target] {
                  set selection [$target curselection]
                  global $target.var
                  if {$value == "EMPTY_ENV_STRING"} {
                    set $target.var ""
                    $target selection set 0
                    event generate $target <<ListboxSelect>>
                  } elseif {$selection != ""} {
                    set selected [$target get $selection]
                    set $target.var $value
                    set newpos [lsearch -exact $value $selected]
                    if {$newpos == -1} {
                      $target selection clear 0 end
                      event generate $target <<ListboxSelect>>
                    } else {
                      $target selection clear 0 end
                      $target selection set $newpos
                    }
                  } else {
                    set $target.var $value
                  }
                }
              }    
            }
            select_first_list_box {
              catch {
                if [valid_handler_name $target] {
                  global $target.var
                  if {$value == "EMPTY_ENV_STRING"} {
                    set $target.var ""
                    $target selection set 0
                    event generate $target <<ListboxSelect>>
                  } else {
                    set $target.var $value
                    $target selection clear 0 end
                    $target selection set 0
                    event generate $target <<ListboxSelect>>
                  }
                }
              }    
            }
            env_window {
              catch {
                process_env_window $target $value
              }
            }
            default {
              report_status "unhandled update $data"
            }
          }
        } else {
          report_status "invalid update: $data"
        }
      }
      register {
        set_handler_name $arg1 $rest
      } 
      close {
        set time_to_exit 0
        shut_it_down 1
      }
      default {
        report_status "Invalid command : $data"
      }
    }
  } else {
       report_status "Malformed command: $data"
  }
}  

# send_environment_cmd
# This procedure sends the command (cmd) passed in to the Lisp
# server.  There is no error checking on the command sent.

proc send_environment_cmd {cmd} {
  # don't know how to test if the socket is still active
  # before trying this, perhaps just wrapping it in a catch
  # is good enough, but it doesn't seem to help if the socket goes
  # down.

  global environment_socket
         
  catch {
    puts $environment_socket "($cmd)<end>"
  }
}

# accept_connection
# This function gets called when a socket connects to the listening
# socket. 
# The parameters are the socket for communicating (sock), and the
# address (addr) and port (port) of the remote connection.  Since 
# there is no handshaking the address and port of the remote aren't
# used, but perhaps for a secure server such checks may be necessary.
# The listening socket is closed, because the environment only needs
# one connection.  Then the real communication socket is configured
# so that it doesn't block or buffer and that the accept_socket_commands
# function gets called everytime there is data to read from that socket.

proc accept_connection {sock addr port} {
  global connection_socket
  global environment_socket
  fconfigure $sock -blocking no
  fconfigure $sock -buffering none
  fileevent $sock readable "accept_socket_commands $sock"
  close $connection_socket
  set connection_socket ""
  set environment_socket $sock
}

# environment_socket is the global variable that holds the socket
# connected to Lisp.

set environment_socket ""

# This is where the listening server gets started.
# Just create a server that calls accept_connection when
# a connection is made on the port specified in the net-config file.
# That's all there is to it!

set connection_socket [socket -server accept_connection $tcl_port]


# These procedures are ones that I needed for the declarative viewer
# and it seemed like they'd be generally useful for other dialogs 
# as well so I've included them here.

# updater
# Given a tcl window (w) if that window has a Lisp handler
# associated with it then send a command to that handler to 
# update this window.

proc updater {w} {
  if [valid_handler_name $w] {
    send_environment_cmd "update [get_handler_name $w]"
  }
}


# remove_handler
# Given a tcl window (w) if that window has a Lisp handler
# send the command to remove that handler and remove the 
# entry for this window from the global handler table.
# You can also pass the optional removal function (f)
# which will be sent as part of the remove command.

proc remove_handler {w {f ""}} {
  global handler_names
   
  if [valid_handler_name $w] {
    send_environment_cmd "remove [get_handler_name $w] $f"
    array unset handler_names $w
  }
}

# setchoice
# This function takes 2 parameters, which are a drop_down listbox (win)
# and a flag to indicate whether or not this choice is a real one (valid)
# If valid is true then the global variable associated with this listbox
# is set to the index of the currently selected item and if valid is false
# the global is set to -1.  The global variable is the target of a tkwait, 
# which is why it needs to be set even for a "nonchoice"

proc setchoice {win valid} {
  global $win.choice
     
  if $valid {
    if {[$win curselection] == ""} {
      set $win.choice -1
    } else {
      set $win.choice [$win curselection]
    }
  } else {
    set $win.choice -1
  }
}


# The following is a hack that gets around a race condition
# in an idiom that I was using and don't have a better solution for.
# the race condition was this:
# if {$var == ""} {tkwait variable var}
# where var was going to get set by the socket handlers
# it became a real problem on a 100MHz Pentium rig running Linux
# and maybe it's causing some of my Mac troubles, who knows.
# the hack is to replace that with a polling loop that blocks for
# 100 ms at a crack so as to not grind the processor checking

global hack_ticker

proc set_hack_ticker {} {
  global hack_ticker
  set hack_ticker 1
  after 100 {set_hack_ticker}
}

after 100 {set_hack_ticker}

proc wait_for_non_null {var} {
  global hack_ticker  
  
 # report_status "waiting for $var"

  upvar $var check
  while {$check == ""} {
    tkwait var hack_ticker
  }
  # report_status "-done  "
}



# source all the dialog folder .tcl files.
# They're sourced in sorted order, so prepending a number to the
# beginning of the name helps enforce an order on them.


cd dialogs

foreach f [lsort [glob -nocomplain *.tcl]] {source $f}

cd ..


# there's got to be a connection at this point since the init dialog requires 
# it, so now we just need to send the keep alive periodically to make sure
# that the socket doesn't timeout (seems to only be a problem in MCL because
# the active sockets don't allow a timeout parameter so they die if inactive
# for more than 30 seconds, but it can't really hurt to do it every where - 
# one small message every 20 seconds seems pretty insignificant)


proc keep_alive {} { 
  send_environment_cmd "k-a"
  after 20000 keep_alive 
}         

# Get things rolling with the keep_alives...

after 20000 {keep_alive}


