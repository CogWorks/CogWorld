;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
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
;;; Filename    : handlers.lisp
;;; Version     : 2.0
;;; 
;;; Description : No system dependent code.
;;;             : Defines the class for the environment "handlers", 
;;;             : the function that processes the incoming messages, and
;;;             : the functions that build the Lisp->Tcl messages.
;;; Bugs        : 
;;; 
;;; Todo        : 
;;; 
;;; ----- History -----
;;;
;;; 05/10/2002  Dan
;;;             : Added this header
;;; 05/20/2002  Dan
;;;             : Think I've finally got it working on Macs w/ OS < 10 and MCL.
;;; 05/22/2002  Dan
;;;             : Moved the system dependent code out of here.
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;;             : Added the simple-text-handler class.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; -------------------------------------------------------------------------
;;; 2005.04.12  Dan [2.0]
;;;             : * Moving to ACT-R 6.
;;;             :
;;; 2007.08.10  Dan
;;;             : * Adding a new class of handlers - a simple-funcall.  It
;;;             :   works like a simple handler on the Lisp side but considers
;;;             :   the target to be a procedure name that gets called on the Tcl/Tk 
;;;             :   side with the value of the update.
;;; 2007.08.13  Dan
;;;             : * Added the removal from the new reset-hook-list to the
;;;             :   delete-handler code.
;;; 2008.05.16  Dan
;;;             : * Updated delete-handler to remove the conflict-nil hooks.
;;; 2008.08.20  Dan
;;;             : * Added a new handler text-handler.  Unlike the simple-text
;;;             :   handler this one always erases the target box on the Tcl
;;;             :   side.
;;; 2009.04.13  Dan
;;;             : * Uni-send-string doesn't have an automatic newline now,
;;;             :   so need to put one in the string to be sent.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;(in-package :cl-user)

;#+(and :allegro-ide (not :ACTR-ENV-ALONE)) (in-package :cg-user)

;;; This is the table that holds handlers indexed by thier names

(defvar *handler-table* (make-hash-table))

;;; This is a list of the handlers for exp windows on the
;;; Tcl side because any change needs to update all of them.

(defvar *env-windows* nil)

;;; class: environment-handler
;;; This class is for communication between objects on the Tcl
;;; side and here in Lisp.  Every object that exists on the Tcl
;;; side and needs to get data from Lisp will have a corresponding
;;; instance of some subclass of this class.  
;;; The slots are:
;;; name - a unique name for each instance used by Tcl for reference
;;;        and as the key into a hashtable.  When the handler is
;;;        removed the name is uninterned.
;;; object-name, target-name and update-form are the corresponding
;;;    elements from the Tcl -> Lisp create message and update messages 
;;;   (see messages.txt for full details)
;;; update-type is one of the valid update-types from the Lisp -> Tcl
;;;   update message (again see messages.txt for full details)
;;; update-value is set every time an update is sent to Tcl with the
;;;   value piece of the Lisp -> Tcl update.
;;; socket is the TCP socket on which this handler is reading and writing.

(defclass environment-handler ()
  ((name :accessor name :initform (gentemp "HANDLER"))
   (object-name :accessor obj-name :initarg :object-name)
   (target-name :accessor target-name :initarg :target-name)
   (update-form :accessor update-form :initarg :update-form)
   (update-type :accessor update-type :initarg :update-type)
   (update-value :accessor update-value)
   (socket :accessor socket :initarg :socket)))

;;; a simple handler responds with simple updates

(defclass simple-handler (environment-handler)
  ()
  (:default-initargs
    :update-type 'simple))

;;; a list handler also responds with simple updates
;;; but has to form them slightly differently

(defclass list-handler (simple-handler)
  ())

;;; a simple-funcall handler responds with simple_funcall updates
;;; otherwise works like a simple handler

(defclass simple-funcall-handler (simple-handler)
  ()
  (:default-initargs
    :update-type 'simple_funcall))


;;; a text-handler is a simple handler which draws
;;; it's string return value into a text box erasing
;;; what's already there.

(defclass text-handler (simple-handler)
  ()
  (:default-initargs
    :update-type 'text))

;;; a simple-text handler works just like a simple handler
;;; except that on the other side it gets handled a little
;;; differently

(defclass simple-text-handler (simple-handler)
  ()
  (:default-initargs
    :update-type 'simpletext))


;;; a window handler passes a list of commands but gets
;;; treated special on the other side

(defclass env-window-handler (list-handler)
  ()
  (:default-initargs
      :update-type 'env_window))

(defmethod initialize-instance :after ((handler env-window-handler) &key)
  (push handler *env-windows*))

;;; a listbox needs a special list handler

(defclass list-box-handler (list-handler)
  ()
  (:default-initargs
      :update-type 'list_box))

;;; and there's a special handler for listboxes that will
;;; always result in the first item being selected on an
;;; update

(defclass select-first-list-box-handler (list-handler)
  ()
  (:default-initargs
      :update-type 'select_first_list_box))


;;; an output handler gets it's values by capturing *standard-output*
;;; and *command-trace*

(defclass output-handler (environment-handler)
  ())

;;; a simple output handler just sends that output 

(defclass simple-output-handler (output-handler)
  ()
  (:default-initargs
      :update-type 'simple))

;;; a 'special' simple output handler doesn't
;;; "undo" the ENPTY_ENV_STRING on the Tcl side

(defclass special-simple-output-handler (simple-output-handler)
  ()
  (:default-initargs
    :update-type 'special))


;;; a text handler means that the output becomes the entire contents
;;; of a text box, so it does some extra work on the Tcl side

(defclass text-output-handler (output-handler)
  ()
  (:default-initargs
    :update-type 'text))

;;; safe-update-evaluation
;;; because the handlers get functions from Tcl that are
;;; then 'apply'ed to produce a result there's the potential for
;;; errors in either the generation or transmission of those expressions
;;; so this function performs the evaluation of a handlers update-form
;;; making sure to catch any errors and print a message if an error occurs.
;;; It returns 2 values on a successful eval the first is the result of that
;;; eval and the second is t.  If there is any condition generated by the
;;; eval then it returns 2 nils.

(defmethod safe-update-evaluation ((handler environment-handler) arg)
  (multiple-value-bind (result err)
      (ignore-errors (apply (update-form handler) (list arg)))
    (if (and (subtypep (type-of err) 'condition)
             (not (equal (type-of err) 'unbound-variable)))
        (progn
          (format t "~S~%" (type-of err))
          (uni-report-error 
           err (format nil 
                         "Error in update of Handler: ~s~%For object: ~s~%message: ~s~%"
                 (name handler)
                 (obj-name handler)
                 (update-form handler)))
          
          (values nil nil))
      (values result t))))
  
(defgeneric update-handler (handler arg)
  (:documentation  "The method called when an environment 
                    item needs to be updated.  The arg
                    is the argument passed to the hook function
                    if there is such an argument."))

;;; for a simple-handler just save the result and then send it

(defmethod update-handler ((handler simple-handler) arg)
  (multiple-value-bind (result success)
       (safe-update-evaluation handler arg)
    (when success
      (setf (update-value handler) result)
      (send-update handler)
      result)))

;;; for a list handler verify that the result is a list, and if so
;;; set the update-value to be a string containing a Tcl list of those
;;; elements otherwise set the update value to "EMPTY_ENV_STRING"
;;; and then send the update

(defmethod update-handler ((handler list-handler) arg)
  (multiple-value-bind (result success)
      (safe-update-evaluation handler arg)
    (when success
      (setf (update-value handler) (if (and result (listp result))
                                       (string-downcase 
                                        (format nil "~{~S ~}" result))
                                     "EMPTY_ENV_STRING"))
      (send-update handler)
      result)))

;;; for the environment window handler I don't want to downcase
;;; the parameters, but other than that it's like the list-handler

(defmethod update-handler ((handler env-window-handler) arg)
  (multiple-value-bind (result success)
      (safe-update-evaluation handler arg)
    (when success
      (setf (update-value handler) (if (and result (listp result))
                                       (concatenate 'string
                                         (string-downcase (format nil "~S " (car result)))
                                         (format nil "~{~S ~}" (cdr result)))
                                     "EMPTY_ENV_STRING"))
      (send-update handler)
      result)))


;;; for an output-handler capture the output to *standard-output* and
;;; the ACT-R *command-trace*.  If there is any output set the value to
;;; that otherwise set the update value to "EMPTY_ENV_STRING" and
;;; then send the update

(defmethod update-handler ((handler output-handler) arg)
  (let* ((s (make-string-output-stream))
         (*standard-output* s)
         (c-trace (car (no-output (sgp-fct (list  :cmdt))))))
    (sgp-fct (list :cmdt s))
    (multiple-value-bind (result success)
        (safe-update-evaluation handler arg)
      (when success
        (setf (update-value handler) (get-output-stream-string s))
        (when (zerop (length (update-value handler)))
          (setf (update-value handler) "EMPTY_ENV_STRING"))
        (send-update handler)
        result))
    (sgp-fct (list :cmdt c-trace))))
    

;;; send_update
;;; this method builds the string containing a Lisp -> Tcl update message
;;; as described in messages.txt for a handler and sends it to Tcl

(defmethod send-update ((handler environment-handler))
  (let ((*print-case* :downcase))
    (send-environment-message handler (format nil "update ~a ~a ~a" 
                                        (update-type handler) 
                                        (target-name handler)
                                        (update-value handler)))))
;;; send_register
;;; this method builds the string containing a Lisp -> Tcl register message
;;; as described in messages.txt for a handler and sends it to Tcl

(defmethod send-register ((handler environment-handler))
  (let ((*print-case* :downcase))
    (send-environment-message handler (format nil "register ~a ~a"
                                        (obj-name handler)
                                        (name handler)))))



;;; delete-handler
;;; This method takes one parameter which is an environment-handler.
;;; That handler is removed from all of the update hooks, removed from
;;; the global table of handlers, and its name is uninterned to free
;;; any possible garbage associated with it.

;;; Assumes that handlers with a hook other than create can
;;; only be created in the context of a model


(defmethod delete-handler ((handler environment-handler))
  (let ((env-mod (when (current-model) 
                   (get-module :environment))))
    (remhash (name handler) *handler-table*)
    
    (when env-mod
      (setf (env-mod-pre-hook-list env-mod)
        (remove handler (env-mod-pre-hook-list env-mod)))
      (setf (env-mod-post-hook-list env-mod)
        (remove handler (env-mod-post-hook-list env-mod)))
      (setf (env-mod-conflict-hook-list env-mod)
        (remove handler (env-mod-conflict-hook-list env-mod)))
      (setf (env-mod-conflict-nil-hook-list env-mod)
        (remove handler (env-mod-conflict-nil-hook-list env-mod))))
    
    (setf *creation-hook-list* (remove handler *creation-hook-list*))
    
    (setf *deletion-hook-list* (remove handler *deletion-hook-list*))
    (setf *reset-hook-list* (remove handler *reset-hook-list*))
    
    (setf *env-windows* (remove handler *env-windows*))
  
    (unintern (name handler))
    (setf (name handler) nil)))


;;; send-environment-message 
;;; This method takes two parameters the first is an environment-handler
;;; and the second is a string of a message to send.  That message is
;;; printed down the socket-stream associated with the handler with the
;;; <end> tag attached.

(defmethod send-environment-message ((handler environment-handler) message)
  ;(format t "sent message: ~S~%" message)
  (multiple-value-bind (value condition)
      (ignore-errors   
       (uni-send-string (socket handler) (format nil "~a<end>~%" message))) 
    (declare (ignore value))
    (when (subtypep (type-of condition) 'condition)
      (uni-report-error condition 
                        (format nil "~S Failed while sending message:~%~S~%"
                          (name handler) message)))))



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
