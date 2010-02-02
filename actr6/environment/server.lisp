;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : server.lisp
;;; Version     : 2.0
;;; 
;;; Description : Contains no system dependent code because it relies on the
;;;             : definitions in uni-files.
;;;             : Holds the Environment global variables and the
;;;             : code to open and manage an environment socket connection. 
;;; Bugs        : 
;;; 
;;; Todo        : Reconstruct why all the error trapping ignores the
;;;             : unbound-variable errors because that's a bad thing
;;;             : for the stand-alone version...
;;; 
;;; ----- History -----
;;;
;;; 05/10/2002  Dan
;;;             : Added this header
;;; 05/20/2002  Dan
;;;             : Think I've finally got it working on Macs w/ OS < 10 and MCL
;;;             : and for now w/OSX it's got to be OpenMcl which also seems
;;;             : to work (though it's not in here yet...)
;;; 05/21/2002  Dan
;;;             : Serious reorganization took place with the files...
;;; 05/22/2002  Dan
;;;             : Fixed some more MCL problems.  At this point it's working
;;;             : mostly, so I'm not wasting any more time 'fixing' it there
;;;             : since MacOS < 10 is "dead"...
;;;             : Changed close-connection so that killing the process was
;;;             : optional and altered the message-process and process-
;;;             : connection functions so that they don't kill their own
;;;             : process (doesn't seem to change things, but seemed like a
;;;             : good idea and I thought it'd fix a closeing bug I had
;;;             : in MCL, but it didn't).
;;; 05/23/2002  Dan
;;;             : Made a very subtle change, that may have a big impact
;;;             : later on.  When a handler is created it preforms an initial
;;;             : update - that's as it was, but now that first time the
;;;             : handler itself is passed as the parameter to the updater.
;;;             : Thus if an updater needs to use its parameter it had better
;;;             : check its type because that first time it'll be something
;;;             : other than it normally expects.
;;; 08/23/2002  Dan
;;;             : Updated the version number to 6.0b3, which doesn't 
;;;             : correspond to the versions in the headers, so I
;;;             : need to fix that at some point for consistency...
;;; 09/17/2002  Dan
;;;             : Updated the environment and ACT-R version numbers...
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;;             : Added the conditionalized in-package to message-process
;;;             : to make building a standalone easier.
;;;             : Changed the environment version to correspond to the
;;;             : file version numbers and add a -S if it's the standalone.
;;; 01/20/2003  Dan
;;;             : Updated version number again for the env because
;;;             : of a quick change for the class.
;;; 07/07/2003  Dan
;;;             : Updated version because I rolled in most of Mike's newest
;;;             : RPM (my UWI and ACL device files were newer).
;;; 08/15/2003  Dan
;;;             : Updated version to 1.3 because I've added the support for
;;;             : CMUCL from Ethan Glasser-Camp at RPI.
;;; 12/19/2003  Dan
;;;             : Updated version to 1.4 for this file because I've fixed
;;;             : a bug in the uni-files and moved the ACT-R version
;;;             : variable to the actr5.lisp file itself.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info, updated the version to 1.5
;;;             : and removed the ACT-R version variable since that's
;;;             : in the main ACT-R file now.
;;; -----------------------------------------------------------------------
;;; 2005.04.11  Dan [2.0]
;;;             : Start of the move to ACT-R 6.0.
;;; 2007.08.03  Dan
;;;             : * Moved the *environment-sockets* defvar to env-module
;;;             :   to avoid a compiler warning.
;;; 2007.08.13  Dan
;;;             : * Adding reset as a possible update condtion for handlers.
;;; 2007.08.17 Dan
;;;             : * Added a without-interrupts call to close-connection to
;;;             :   fix a problem in stopping things under LispWorks.
;;; 2008.01.15 Dan
;;;             : * Changed close-connection because SBCL doesn't have
;;;             :   without-interrupts in the default package...
;;; 2008.04.08 Dan
;;;             : * Refixed that last update using uni-without-interrupts.
;;;             :   Also added the require-compiled of uni-files just to be
;;;             :   safe.
;;; 2008.05.16 Dan
;;;             : * Added the conflict-nil option for when to update the
;;;             :   handler.  That's the same as conflict except that it 
;;;             :   doesn't pass the return value back to the conflict-set-
;;;             :   hook.
;;; 2009.01.08 Dan
;;;             : * Fixed an issue with message-process which could cause
;;;             :   problems with some Lisps (CCLx86 in particular).
;;; 2009.04.13  Dan
;;;             : * Uni-send-string doesn't have an automatic newline now,
;;;             :   so need to put one in the string to be sent.
;;; 2009.04.14  Dan
;;;             : * Seems that adding the newline here breaks the stopping
;;;             :   of the connection.  However, since it doesn't seem to be 
;;;             :   necessary I'm just dropping it again.
;;; 2009.06.08  Dan
;;;             : * Adding the run-environment command which spawns the
;;;             :   environment app and then makes the connection.
;;;             :   Only available for LispWorks under MacOSX or Windows and
;;;             :   ACL under MacOSX or Windows at this time.
;;; 2009.06.09 Dan
;;;             : * Upped the default delay on run-environment to 12 seconds
;;;             :   to be safer.
;;; 2009.07.17 Dan
;;;             : * Added a test to the remove handler code so that it 
;;;             :   shouldn't throw an error on initial connection anymore
;;;             :   like it would occasionally for some Lisps.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(require-compiled "UNI-FILES" "ACT-R6:support;uni-files")


;;; These are the address and ports that will be used
;;; for communication with the Tcl environment.
;;; There must be a passive listener at *tcl-address*:*tcl-port*
;;; to recieve calls from Lisp.  The port has to match the value set in the
;;; 0-net-config.tcl file so that the 2 systems can communicate if the 
;;; default values are used for connect-to-environment.

(defparameter *tcl-port* 2621)
(defparameter *tcl-address* "127.0.0.1")



;;; a global that'll be shadowed in each of the handler threads
;;; which indicates 

(defvar *local-connection* nil)

;;; close-connection
;;; This function takes one parameter which should be a socket stream, and
;;; a keywork parameter kill which defaults to t.  If kill is t, then
;;; it kills the process that is reading from that stream. It always
;;; removes all the handlers in the table that are associated with that 
;;; stream and then closes the stream.

(defun close-connection (stream &key (kill t))
  (let ((connection (find stream *environment-sockets* 
                          :key #'car :test #'equal)))
    (when kill
      (uni-process-kill (second connection)))
    (uni-without-interrupts
      (ignore-errors 
        (uni-send-string (car connection) "close nil nil<end>")))
    
    

    (setf *environment-sockets* 
      (remove stream *environment-sockets* :key #'car :test #'equal))
    
    (maphash #'(lambda (key value) 
                 (declare (ignore key))
                 (when (equal (socket value) stream) 
                   (delete-handler value)))
             *handler-table*)
    
    (ignore-errors 
     (close stream))))

;;; close-all-connections
;;; This function takes no parameters. It kills all of the processes
;;; that are handling environment connections, closes all of the 
;;; sockets associated with them and removes all of the handlers from
;;; the table.

(defun close-all-connections ()
  (dolist (connection *environment-sockets*)
    (close-connection (car connection)))
  (setf *environment-sockets* nil)
  (setf *creation-hook-list* nil)
  (setf *deletion-hook-list* nil)
  (setf *reset-hook-list* nil))

  
;;; connect-to-environment 
;;; It takes 3 keyword parameters. Clean specifies whether or not to close 
;;; all open environment socket connections before making the new one, and 
;;; it defaults to t.  The keywords host and port specify the address of 
;;; the Tcl environment listening for a connection and default to the
;;; global variables specifed above.  
;;; This function opens a socket connection to that Tcl environment and starts
;;; a process that will read and process the input on that socket.

(defun connect-to-environment (&key (clean t)
                                     (host *tcl-address*) 
                                     (port *tcl-port*))
  (when clean
    (close-all-connections))
  
    (multiple-value-bind (s err) 
        (ignore-errors (uni-make-socket host port))
      (if (and (subtypep (type-of err) 'condition)
               (not (equal (type-of err) 'unbound-variable)))
          (uni-report-error err "Unable to Connect")
        (push (list s 
                    (uni-run-process
                     "Environment-Connection" 
                     #'(lambda ()
                         (message-process s (if (string-equal host "127.0.0.1")
                                                1 0))))
                    (cons host port))
              *environment-sockets*))))
  
;;; message-process
;;; This function takes two parameters the first is a socket stream to read 
;;; commands from and the second is whether or not the environment it's 
;;; connecting to are on the same machine (1 or 0 for t or nil).
;;; It reads the Tcl->Lisp commands from the socket (see
;;; the messages.txt file for details) and creates a process to handle
;;; each one as it arrives.  If there is a connection error or the socket
;;; is closed then this function terminates.

(defun message-process (input-stream local-environment)
  #+(and :allegro :ACTR-ENV-ALONE) (in-package :cl-user)
  
  (let ((old-string "")) ;; be careful about new lines with Scott's read-line
    (loop 
      (unless (uni-wait-for-char input-stream)
        (return))

      ;; read a line from the socket
        (let ((current-string (multiple-value-bind (value condition)
                                  (ignore-errors   
                                   (uni-socket-read-line input-stream)) 
                                (if (subtypep (type-of condition) 'condition)
                                    (progn
                                      (uni-report-error condition 
                                                        "Read line failed")
                                      (format *error-output* 
                                          "Environment Connection ended.~%")
                                      (close-connection input-stream :kill nil)
                                      (return))
                                  value))))
          
          ;(format t "Line read is: ~S~%" current-string)
            
          ;; check for the end marker because the line could have been split
          (if (not (search "<end>" current-string :test #'string-equal))
              (setf old-string (concatenate 'string old-string current-string))
            
            (multiple-value-bind (cmd-list condition)
                (ignore-errors  
                 (read-from-string 
                  (concatenate 'string old-string current-string) 
                  nil 'problem)) 
              
              (setf current-string 
                (concatenate 'string old-string current-string))
              (setf old-string "")
              
              
              ;(format t "cmd-list is:~S~%" cmd-list)
              ;(format t "current-string is:~S~%" current-string)
              
              (cond ((subtypep (type-of condition) 'condition) ;; an error
                     (uni-report-error 
                      condition (format nil "Error reading from message: ~s"
                                  current-string))
                     ; not closing connection anymore, maybe still should ?
                     ;(format *error-output* 
                     ; "Closing connection to environment~%")
                     ;(close-connection input-stream)
                     ;(return)
                     )
                      
                    ((equal 'problem cmd-list) ;; not likely to occur now
                     (format *error-output* "Environment Closed~%")
                     (close-connection input-stream :kill nil)
                     (return))
                    ((listp cmd-list) ;; any list is assumed to be a good cmd
                     (let ((local (copy-tree cmd-list)))
                       (uni-run-process 
                        "Environment-Handler" 
                        #'(lambda () 
                            (let ((*local-connection* local-environment))
                              (process-connection input-stream local))))
                       ;(format t "new-process spawned~%")
                       ))
                    (t ;; anything else 
                     (format *error-output* 
                         "Incorrect command recieved: ~S~%" 
                       current-string)))))))))


;;; process-connection
;;; This function takes 2 parameters input-stream which should be a socket
;;; stream from which a message was read, and cmd-list which is the list
;;; containing that message.  This function gets called in a separate process
;;; for each message sent and does what the message requests - create a new
;;; handler, remove a handler, update a handler, or just keep the connection
;;; alive (an MCL issue).

;;; Big assumption 
;;; Hooks other than create can only be assigned in
;;; the context of a model...


(defun process-connection (input-stream cmd-list) 
  ;(format t "~%process-connection:~%")
  (case (car cmd-list) 
    (create ;; make a new handler instance and send a register + update back
     (if (= (length cmd-list) 6)
         (let ((env-mod (when (current-model)
                          (get-module :environment)))
               (new-handler (make-instance (second cmd-list) 
                              :socket input-stream
                              :object-name (third cmd-list) 
                              :target-name (fourth cmd-list)
                              :update-form (functionify (fifth cmd-list)))))
           (setf (gethash (name new-handler) *handler-table*) 
             new-handler)
           (send-register new-handler)
           (update-handler new-handler new-handler)
           (dolist (x (sixth cmd-list))
             (case x
               (pre (when env-mod
                      (push new-handler (env-mod-pre-hook-list env-mod))))
               (post (when env-mod
                       (push new-handler (env-mod-post-hook-list env-mod))))
               
               (conflict (when env-mod
                           (push new-handler (env-mod-conflict-hook-list env-mod))))
               (conflict-nil (when env-mod
                           (push new-handler (env-mod-conflict-nil-hook-list env-mod))))
               (create (push new-handler *creation-hook-list*))
               (delete (push new-handler *deletion-hook-list*))
               (reset (push new-handler *reset-hook-list*))
               (t (model-warning "Invalid hook for handler ~S" cmd-list)))))
       (format *error-output* "Invalid create message: ~s" cmd-list)))
    (update ;; change the update form if requested and send an update back
     (cond ((= (length cmd-list) 2)
            (let ((handler (gethash (second cmd-list) *handler-table*)))
              (if handler
                  (update-handler handler nil)
                (format *error-output* "Warning: update for removed handler ~S~%"
                  (second cmd-list)))))
           ((= (length cmd-list) 3)
            (let ((handler (gethash (second cmd-list) *handler-table*)))
              (if handler
                  (progn
                    (setf (update-form handler) (functionify (third cmd-list)))
                    (update-handler handler nil))
                (format *error-output* "Warning: update for removed handler ~S~%"
                  (second cmd-list)))))
           (t
            (format *error-output* "Invalid update message: ~s" cmd-list))))
    (remove ;; take the handler off the lists and free its name
            ;; calling the optional end funciton if necessary
     (cond ((= (length cmd-list) 2)
            (let ((handler (gethash (second cmd-list) *handler-table*)))
              
              (while (null handler)
                (uni-process-system-events)
                (setf handler (gethash (second cmd-list) *handler-table*)))
              
              (delete-handler handler)))
           ((= (length cmd-list) 3)
            (let ((handler (gethash (second cmd-list) *handler-table*)))
              (while (null handler)
                (uni-process-system-events)
                (setf handler (gethash (second cmd-list) *handler-table*)))
              
              (safe-evaluation (third cmd-list) handler)
              (delete-handler handler)))
           (t (format *error-output* "Invalid remove message: ~s" 
                cmd-list))))
    (k-a ;; don't do anything - just to make sure the socket doesn't timeout
     )
    (goodbye ;; kill the connection
     (format t "Environment Closed~%")
     (close-connection input-stream :kill nil))
    (t 
     (format *error-output* "Invalid command request: ~s~%" cmd-list)))
  ;(format t "process-connection done~%")
  )

(defmethod safe-evaluation (form handler)
  (multiple-value-bind (result err)
      (ignore-errors (funcall (functionify form)))
    (if (and (subtypep (type-of err) 'condition)
             (not (equal (type-of err) 'unbound-variable)))
        (progn
          (format t "~S~%" (type-of err))
          (uni-report-error 
           err (format nil 
                         "Error in remove message for: ~S%Removing: ~S~%Message: ~S~%"
                 (name handler)
                 (obj-name handler)
                 form))
          
          (values nil nil))
      (values result t))))



;;; The "typical" user will use the following functions to connect/disconnect
;;; to the environment.

(defun start-environment ()
  (if *environment-sockets*
      (format t "There is already a connection to the environment.~%You should either stop that first, or if you want to connect to a second or remote environment use the connect-to-environment command~%")
    (connect-to-environment)))

(defun stop-environment ()
  (if (> (length *environment-sockets*) 1)
      (format t "There is more than one environment currently connected.~%You must use either close-all-connections to stop them all or close-connection to stop a specific one.~%")
    (close-all-connections)))


(defun run-environment (&optional (delay 0))
  (declare (ignore delay))
  (print-warning "The run-environment command is not available for your current Lisp & OS combination."))

#+(and :lispworks (or :win32 :win64) :lispworks5)
(defun run-environment (&optional (delay 12))
  (sys:call-system "\"Start Environment.exe\"" :current-directory (translate-logical-pathname "ACT-R6:environment") :wait nil)
  (sleep delay)
  (start-environment))

#+(and :lispworks :macosx)
(defun run-environment (&optional (delay 12))
  (sys:call-system (format nil "'~a/Start Environment OSX.app/Contents/MacOS/Wish Shell'"
                     (namestring (translate-logical-pathname "ACT-R6:environment")))
                   :wait nil)
  (sleep delay)
  (start-environment))


#+(and :allegro :mswindows)
(defun run-environment (&optional (delay 12))
  (let ((c (current-directory)))
    (chdir "ACT-R6:environment")
    (run-shell-command "\"Start Environment.exe\"" :wait nil)
    (chdir c))
  (sleep delay)
  (start-environment))

#+(and :allegro :macosx)
(defun run-environment (&optional (delay 12))
  (let ((c (current-directory)))
    (chdir "ACT-R6:environment")
    (run-shell-command "'Start Environment OSX.app/Contents/MacOS/Wish Shell'" :wait nil)
    (chdir c))
  (sleep delay)
  (start-environment))


#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
