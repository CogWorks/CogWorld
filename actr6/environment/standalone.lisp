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
;;; Filename    : standalone.lisp
;;; Version     : 1.5
;;; 
;;; Description : Contains the code for connecting an application
;;;             : version of the environment to the Tcl side.
;;; Bugs        : 
;;; 
;;; Todo        : [ ] Get rid of those global pathname variables.
;;; 
;;; ----- History -----
;;;
;;; 10/01/2002  Dan
;;;             : File creation
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; 2005.05.12 Dan
;;;             : * Added the safe-load and smart-load functions so that
;;;             :   the standalone can have the open dialog.
;;; 2008.08.21 Dan
;;;             : * Added code for the ACL version of the standalone to 
;;;             :   automatically exit the error which can get generated
;;;             :   during a define-model call.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(in-package :cl-user)

;#+(and :allegro-ide (not :ACTR-ENV-ALONE)) (in-package :cg-user)

(defvar *global-output-stream*  (make-string-output-stream))
(defvar *stop-environment* nil)

(defun run-standalone ()
  (setf *chunk-type-file* (merge-pathnames "chunk-type.lisp" *install-dir*))
  (setf *chunk-file* (merge-pathnames "chunk.lisp" *install-dir*))
  (setf *production-file* (merge-pathnames "production.lisp" *install-dir*))
  (setf *misc-file* (merge-pathnames "misc.lisp" *install-dir*))
  (setf *command-file* (merge-pathnames "command.lisp" *install-dir*))
  
#+(and :allegro :actr-env-alone) (setf *debugger-hook* 'standalone-debug-exit)
  
  (multiple-value-bind (s err) 
        (ignore-errors (uni-make-socket *tcl-address* *tcl-port*))
    (push s *environment-sockets*)
    (if (and (subtypep (type-of err) 'condition)
               (not (equal (type-of err) 'unbound-variable)))
          (uni-report-error err "Unable to Connect")
        (message-process s 1))))


#+(and :allegro :actr-env-alone)
(defun standalone-debug-exit (a b) 
  (model-warning "Error has been cleared automatically") 
  (invoke-restart-interactively (second (compute-restarts))))


(defun eval-command (cmd)
  (let ((*standard-output* *global-output-stream*)
        (*error-output* *global-output-stream*))
    (format t "~%> ~A~%" cmd)
    (multiple-value-bind (result err)
        (ignore-errors (read-from-string cmd))
      (if (and (subtypep (type-of err) 'condition)
             (not (equal (type-of err) 'unbound-variable)))
        (progn
          (format t "~S~%" (type-of err))
          (uni-report-error 
           err (format nil 
                         "Error in command: ~s:~%"
                       cmd))
          
          nil)
        (multiple-value-bind (res err)
        (ignore-errors (eval result))
      (if (and (subtypep (type-of err) 'condition)
             (not (equal (type-of err) 'unbound-variable)))
        (progn
          (format t "~S~%" (type-of err))
          (uni-report-error 
           err (format nil 
                         "Error executing command: ~s:~%"
                       cmd))
          
          nil)
        (progn
          (format t "~%~S~%" res)
          nil)))))))

(defun start-listener-outputer (handler)
   (uni-run-process "Listener-outputer" #'(lambda () (run-listener-outputer handler)))
   "")

(defun run-listener-outputer (handler)
  (loop
    (uni-wait-for #'(lambda ()
                      (let ((text (get-output-stream-string *global-output-stream*)))
                        (if (> (length text) 0)
                            (progn
                              (setf (update-value handler) text)
                              t)
                          nil))))
    (send-update handler)))

#|
Don't want this here do I?
(defun safe-load (file)
  (setf file (create-valid-pathname file))
  (let* ((save-stream (make-string-output-stream ))
         (*standard-output* save-stream)
         (*error-output* save-stream))
    (multiple-value-bind (s err) 
        (ignore-errors (load file))
      (declare (ignore s))
      (if (and (subtypep (type-of err) 'condition)
               (not (equal (type-of err) 'unbound-variable)))
          (progn
            (uni-report-error err (format nil "Error loading file: ~S~%" file))
            (list 0 (get-output-stream-string save-stream)))
        (list 1 (get-output-stream-string save-stream))))))
|#

#+:ACTR-ENV-ALONE
(defun smart-loader (file)
  (declare (ignore file))
  (list 0 "You must disable the 'Compile definitions when model opened or reloaded' option to be able to open a model"))

#-:ccl-5.0 

(defun create-valid-pathname (path) path)


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
