
set temp [file split [lindex $auto_path [lsearch -regexp $auto_path ".*(Start Environment OSX.app).*"]]]

set temp [eval "file join [lrange $temp 0 [expr [lsearch -exact $temp "Start Environment OSX.app"] - 1]]"]


cd $temp
cd GUI
source starter.tcl

