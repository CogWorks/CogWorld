# this file generated when environment is closed
# refresh . to make sure sizes are right

wm deiconify .
wm withdraw .
if {[winfo screenwidth .] != 1920 || [winfo screenheight .] != 1200 || [lindex [wm maxsize .] 0] != 1905 || [lindex [wm maxsize .] 1] != 1170} {
  set size_mismatch 1
} else {
  set size_mismatch 0
}

if $size_mismatch {
  set reset_window_sizes [tk_messageBox -icon warning -title "Screen resolution changed" -type yesno \
                                         -message "The screen resolution is not the same as it was the last time the Environment was used.  Should the window positions reset to the defaults?"]
} else { set reset_window_sizes 0}
if {$reset_window_sizes != "yes"} {
  set window_config(.visicon) 805x799+865+43
  set changed_window_list(.visicon) 1
  set window_config(.control_panel) 170x710+1740+275
  set changed_window_list(.control_panel) 1
  set window_config(.copyright) 400x466+760+367
  set changed_window_list(.copyright) 1
}
