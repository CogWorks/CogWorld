;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell 
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c)2002-2004 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : environment-cmds.lisp
;;; Version     : 2.0
;;; 
;;; Description : No system dependent code.
;;;             : Defines stuff that is needed to handle "environment"
;;;             : things.
;;; Bugs        : 
;;; 
;;; Todo        : Clean up what goes here and what's passed literally
;;;             : from the Tcl side - it's an odd mix at this point
;;;             : and I should have some consistency.
;;; 
;;; ----- History -----
;;;
;;; 05/22/2002  Dan
;;;             : File creation
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;; 01/20/2003  Dan
;;;             : Added the show-module-state-chunk function and
;;;             : hacked buffer-contents so that the module-state
;;;             : chunks shown by the environment match internal
;;;             : module states even though the "real" chunks
;;;             : don't seem to be updated anymore. WHY and WHEN?!
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; ----------------------------------------------------------------------
;;; 2005.04.13  Dan [2.0]
;;;             : * Moved to ACT-R 6.
;;; 2007.08.03 Dan
;;;             : * Moved the *stepper-open* defvar here to avoid a warning
;;;             :   at compile time.
;;;             : * Deleted the commented out in-package calls.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;;; Here I define a variable that will be used by the environment
;;; as a simple flag to prevent run from being pressed more than
;;; once (and there may be other uses)

(defvar *running-actr* nil)


;;; Flag to indicate whether the stepper window is open.

(defvar *stepper-open* nil)

;;; Reset-model-env
;;; this is the function called when the reset button in the
;;; environment is pressed.  Basically, all it does print a message
;;; after it resets for reference.

(defun reset-model-env (x)
  (declare (ignore x))
  (if (or *stepper-open* *running-actr*)
      (model-warning "Cannot reset a running model or if the stepper is open.")
    (unwind-protect 
        (progn
          (setf *running-actr* t)
          (reset)
          (format t "~%#|## Model has been reset. ##|#~%"))
      (setf *running-actr* nil))))

;;; Reload-model
;;; this is the function called when the reload button in the
;;; environment is pressed.  It takes one parameter which specifies 
;;; whether or not to use the smart-loader instead of the simple reload
;;; function.

(defun reload-model (smart-load?)
  (let* ((save-stream (make-string-output-stream ))
         (display-stream (make-broadcast-stream *standard-output* save-stream))
         (error-stream (make-broadcast-stream *error-output* save-stream))
         (*standard-output* display-stream)
         (*error-output* error-stream))
    
    (if (or *running-actr* *stepper-open*)
        (list 0 "Cannot reload a model that is running or if the stepper is open.")
      (unwind-protect
          (progn
            (setf *running-actr* t)   
            (multiple-value-bind (s err) 
                (ignore-errors 
                 (if smart-load?
                     (reload t)
                   (reload)))
      
              (cond ((and (subtypep (type-of err) 'condition)
                          (not (equal (type-of err) 'unbound-variable)))
                     (uni-report-error err (format nil "Error reloading model"))
                     (list 0 (get-output-stream-string save-stream)))
                    ((eq s :none)
                     (model-warning "Error Reloading model")
                     (list 0 (get-output-stream-string save-stream)))
                    (t
                     (model-output "Model Reloaded")
                     (list 1 (get-output-stream-string save-stream))))))
        (setf *running-actr* nil)))))

(defun safe-load (file compile-it)
  
  (setf file (create-valid-pathname file))
  
  (let* ((save-stream (make-string-output-stream ))
         (display-stream (make-broadcast-stream *standard-output* save-stream))
         (error-stream (make-broadcast-stream *error-output* save-stream))
         (*standard-output* display-stream)
         (*error-output* error-stream))
    
    
    (multiple-value-bind (s err) 
        (ignore-errors 
         (if compile-it
             (compile-and-load file)
           (load file)))
      (declare (ignore s))
      
      (cond ((and (subtypep (type-of err) 'condition)
                  (not (equal (type-of err) 'unbound-variable)))
             (uni-report-error err (format nil "Error reloading model"))
             (list 0 (get-output-stream-string save-stream)))
            (t
             (model-output "Model Reloaded")
             (list 1 (get-output-stream-string save-stream)))))))
  

;;; buffer-list
;;; This function doesn't take any parameters, and 
;;; doesn't really do anything at this point other than
;;; return a constant list. 
;;; I'm hoping that with 6.0 there'll be a way to add and
;;; access buffers uniformly (unlike now) and there'll exist
;;; a function for getting the list, but for now just hack
;;; it out.

(defun buffer-list (x)
  (declare (ignore x))
  (buffers))

;;; buffer-contents
;;; This function takes one parameter which is a symbol that
;;; is the name of a buffer.  It prints out the chunk that is
;;; currently in that buffer, or "Buffer is Empty" if there isn't
;;; a chunk in that buffer to *standard-output*.
;;; This is another of those mechanisms that'd be nice to fix
;;; for ACT-R 6.0...

(defun buffer-contents (buffer)
  
  (if (buffer-read buffer)
         (buffer-chunk-fct (list buffer))
        
    (format *standard-output* "Buffer is Empty")
  ))


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
