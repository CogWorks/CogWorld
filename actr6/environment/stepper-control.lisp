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
;;; Filename    : stepper-control.lisp
;;; Version     : 2.0
;;; 
;;; Description : No system dependent code.
;;;             : This file contains the Lisp to support the stepper window.
;;;             : 
;;; Bugs        : 
;;; 
;;; Todo        :
;;; 
;;; ----- History -----
;;;
;;; 05/24/2002  Dan
;;;             : File creation
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;;             : Modified the stepper-control-function to use the
;;;             : new uni-wait-for function.
;;; 10/14/2002  Dan
;;;             : Made the changes to implement the tutor mode
;;;             : in the instantiation window instead of the bindings
;;;             : window of the stepper.
;;; 11/11/2002  Dan
;;;             : Modified stepper-instan-info and stepper-control-function
;;;             : so that instantiation picking was possible.  It works
;;;             : off of the step buttons value and the *last-stepper-instantiation*
;;;             : variable.  Requires that the option be set in the
;;;             : environment before it's enabled.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;;             : Updated run-master-process with the RPM 2.2 version.
;;; -----------------------------------------------------------------------
;;; 2005.04.13  Dan
;;;             : * Moved to ACT-R 6.
;;;             : * LOTS of things to change - not included yet.
;;; 2005.04.20  Dan
;;;             : * Updated and added - should work with the tutorial mode.
;;; 2005.05.14 Dan
;;;             : * Fixed a typo in the stepper window - it was printing
;;;             :   +retreval> for retrieval requests...
;;; 2005.08.10 Dan
;;;             : * Minor clean-up to declare event unused in stepper-test.
;;; 2006.03.10 Dan
;;;             : * Calls to get-production no longer need the procedural
;;;             :   module so took that out of stepper-instan-binding.
;;; 2007.07.13 Dan
;;;             : * Added the stepper-stop-button function because the
;;;             :   stop button is being put back on the stepper.
;;; 2007.08.03 Dan
;;;             : * Moved the *stepper-open* defvar to environment-cmds
;;;             :   because it's used there and that's loaded first...
;;; 2007.08.07 Dan
;;;             : * When :esc is set to t the stepper now shows the 
;;;             :   declarative or procedural parameters for the 
;;;             :   item in a new window in the lower left.
;;; 2007.08.08 Dan
;;;             : * Put the "run until" button back into the stepper and
;;;             :   added module as an option now too.
;;; 2007.08.15 Dan
;;;             : * The chunk list shown in the stepper is now sorted
;;;             :   by activation with the chunk being retrieved at the top.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


;(in-package :cl-user)

;#+(and :allegro-ide (not :ACTR-ENV-ALONE)) (in-package :cg-user)


(defvar *stepper-handlers* nil)
(defvar *wait-for-step* nil)
(defvar *current-stepper-event* nil)
(defvar *stepper-viewer-mode* :production)
(defvar *tutor-bindings* nil)
(defvar *tutor-responses* nil)


(defun init-stepper ()
  (setf *stepper-open* t)
  (setf *stepper-handlers* (make-hash-table))
  (setf *wait-for-step* t)
  (setf *current-stepper-event* nil)
  (setf *stepper-viewer-mode* :production)
  (setf *tutor-bindings* nil)
  (setf *tutor-responses* nil)
  (setf *stepper-skip-val* nil) 
  (setf *stepper-skip-type* nil)
  1)

(defun remove-stepper ()
  (setf *stepper-open* nil)
  (setf *wait-for-step* nil)
  (setf *current-stepper-event* nil)
  1)


(defun set-stepper-skip-time (vals)
  (setf *stepper-skip-type* (car vals))
  (setf *stepper-skip-val* (second vals)))
  

(defun stepper-test (event)
  ;; To be expanded with module/buffer based user settings
  (case *stepper-skip-type*
    (production 
     
     (cond ((or (not (symbolp *stepper-skip-val*))
                (not (get-production *stepper-skip-val*)))
            (print-warning "Run Until Production requires a valid production name value.")
            (print-warning "Normal stepping is being used instead.")
            t)
           ((and (or (eq (evt-action event) 'production-selected)
                     (eq (evt-action event) 'production-fired))
                 (eq *stepper-skip-val* (production-name (car (evt-params event)))))
            (setf *stepper-skip-val* nil)
            (setf *stepper-skip-type* nil)
            t)
           (t nil)))
    (time 
     (cond ((not (numberp *stepper-skip-val*))
            (print-warning "Run Until Time requires a number (time in seconds) as the value.")
            (print-warning "Normal stepping is being used instead.")
            t)
           ((>= (evt-time event) *stepper-skip-val*)
            (setf *stepper-skip-val* nil)
            (setf *stepper-skip-type* nil)
            t)
           (t nil)))
    (module 
     (cond ((not (find (string *stepper-skip-val*) (mapcar #'string (all-module-names)) :test #'string-equal) )
            (print-warning "Run Until Module requires a valid module name.")
            (print-warning "Normal stepping is being used instead.")
            t)
           ((string-equal (string *stepper-skip-val*) (string (evt-module event)))
            (setf *stepper-skip-val* nil)
            (setf *stepper-skip-type* nil)
            t)
           (t nil)))
    (t t)))

(defun stepper-step (event)
  (when (and event (act-r-event-p event) 
             (or (event-displayed-p event) 
                 (and (eq *stepper-viewer-mode* :tutor)
                      (eq (evt-module event) 'procedural)
                      (eq (evt-action event) 'production-selected)))
                 
             (stepper-test event))
    (setf *current-stepper-event* event)
    
    (setf *tutor-bindings* nil)
    (dolist (x (hash-table-keys *stepper-handlers*))
      (update-handler x event))
    (while *wait-for-step*
      (uni-process-system-events))
    (dolist (x (hash-table-keys *stepper-handlers*))
      (update-handler x nil))
    (setf *wait-for-step* t)))

(defun add-handler-to-stepper (handler)
  (when (subtypep (type-of handler) 'environment-handler) 
    (unless (gethash handler *stepper-handlers*)
      (setf (gethash handler *stepper-handlers*) t)))
  nil)


(defun stepper-step-button (x)
  (unless x
    (setf *wait-for-step* nil)))


(defun stepper-stop-button (x)
  (schedule-break-relative 0 :priority :max :details "Stopped by the stepper")
  (unless x
    (setf *wait-for-step* nil)))

(defun next-event-display (event)
  (cond ((subtypep (type-of event) 'environment-handler)
         (add-handler-to-stepper event)
         " ")
        ((act-r-event-p event)
         (format-event event))
        (t " ")))

(defun stepper-list-name (event)
  (cond ((subtypep (type-of event) 'environment-handler)
         (add-handler-to-stepper event)
         " ")
        ((act-r-event-p event)
         (case (evt-module event)
           (procedural (case (evt-action event)
                         (PRODUCTION-SELECTED "Possible Productions")
                         (PRODUCTION-FIRED "Production")
                         (t " ")))
           (declarative (case (evt-action event)
                         (retrieved-chunk "Possible Chunks")
                         (t " ")))
           (t " ")))
        (t " ")))

(defun stepper-list-values (event)
  (cond ((subtypep (type-of event) 'environment-handler)
         (add-handler-to-stepper event)
         nil)
        ((act-r-event-p event)
         (let ((env (get-module :environment)))
           (case (evt-module event)
             (procedural (case (evt-action event)
                           (production-selected (env-mod-conflict-set env))
                           (PRODUCTION-FIRED (subseq (env-mod-conflict-set env) 0 1))
                           (t nil)))
             (declarative (case (evt-action event)
                            (retrieved-chunk (if (env-mod-esc env)
                                                 (let ((c (copy-list (env-mod-last-dm-set env))))
                                                   (cons (car c) (sort (cdr c) #'> :key #'(lambda (x) (no-output (caar (sdp-fct (list x :last-retrieval-activation)))))))
                                                   )
                                               (env-mod-last-dm-set env)))
                            (t nil)))
             (t nil))))
        (t nil)))


(defun stepper-prod_frame-name (event)
  (cond ((subtypep (type-of event) 'environment-handler)
         (add-handler-to-stepper event)
         " ")
        ((act-r-event-p event)
         (case (evt-module event)
           (procedural (case (evt-action event)
                         (PRODUCTION-SELECTED "Production")
                         (PRODUCTION-FIRED "Production")
                         (t " ")))
           (declarative (case (evt-action event)
                         (retrieved-chunk "Chunk")
                         (t " ")))
           (t " ")))
        (t " ")))



(defun stepper-instan-production (prod)
  (when prod
    (cond ((act-r-event-p *current-stepper-event*)
           (case (evt-module *current-stepper-event*)
             (procedural (case (evt-action *current-stepper-event*)
                           (PRODUCTION-SELECTED (if (or (eq *stepper-viewer-mode* :production)
                                                        (eq *stepper-viewer-mode* :tutor))
                                                    (pp-fct (list prod))
                                                  (pprint-instantiation-fct prod)))
                           (PRODUCTION-fired (pprint-instantiation-fct prod))
                           (t nil)))
             (declarative (case (evt-action *current-stepper-event*)
                            (retrieved-chunk  (dm-fct (list prod)))
                            (t nil)))
             (t nil)))
          (t nil))))

(defun stepper-parameter-frame-name (event)
  (if (no-output (car (sgp :esc)))
      (cond ((subtypep (type-of event) 'environment-handler)
             (add-handler-to-stepper event)
             " ")
            ((act-r-event-p event)
             (case (evt-module event)
               (procedural (case (evt-action event)
                             (PRODUCTION-SELECTED "Production Parameters")
                             (PRODUCTION-FIRED "Production Parameters")
                             (t " ")))
               (declarative (case (evt-action event)
                              (retrieved-chunk "Chunk Parameters")
                              (t " ")))
               (t " ")))
            (t " "))
    " "))

(defun stepper-parameter-frame-values (prod)
  (when (and prod (no-output (car (sgp :esc))))
    (cond ((act-r-event-p *current-stepper-event*)
           (case (evt-module *current-stepper-event*)
             (procedural (case (evt-action *current-stepper-event*)
                           (PRODUCTION-SELECTED (spp-fct (list prod)))
                           (PRODUCTION-fired (spp-fct (list prod)))
                           (t nil)))
             (declarative (case (evt-action *current-stepper-event*)
                            (retrieved-chunk  (sdp-fct (list prod)))
                            (t nil)))
             (t nil)))
          (t nil))))


(defun tutored-step ()
  (and (act-r-event-p *current-stepper-event*)
       (eq (evt-module *current-stepper-event*) 'procedural)
       (eq (evt-action *current-stepper-event*) 'PRODUCTION-SELECTED)
       (eq *stepper-viewer-mode* :tutor)))
          

(defun stepper-bindings-name (event)
  (cond ((subtypep (type-of event) 'environment-handler)
         (add-handler-to-stepper event)
         " ")
        ((act-r-event-p event)
         (case (evt-module event)
           (procedural (case (evt-action event)
                         (PRODUCTION-SELECTED "Bindings")
                         (PRODUCTION-FIRED "Bindings")
                         (t " ")))
           (declarative (case (evt-action event)
                         (retrieved-chunk "Retrieval Request")
                         (t " ")))
           (t " ")))
        (t " ")))



(defun stepper-instan-binding (prod)
  (when prod
    (let ((env (get-module :environment)))
      (when env
        (cond ((act-r-event-p *current-stepper-event*)
               (case (evt-module *current-stepper-event*)
                 (procedural (case (evt-action *current-stepper-event*)
                               (PRODUCTION-SELECTED (let ((len 0))
                                                      (let ((bindings (production-bindings (get-production prod))))
                                                        (when bindings
                                                          (setf len (apply #'max (mapcar #'(lambda (x)
                                                                                             (length (symbol-name (car x)))) bindings)))
                                                          
                                                          (if (eq *stepper-viewer-mode* :tutor)
                                                              (progn
                                                                (when (null *tutor-bindings*)
                                                                  (setf *tutor-bindings* (copy-tree bindings))
                                                                  (setf *tutor-responses* (mapcar #'(lambda (x) (cons (car x) nil)) *tutor-bindings*)))
                                                                (dolist (x (reverse *tutor-responses*))
                                                                  (command-output "~vA : ~a" len (car x) (if (null (cdr x)) "{binding}" (cdr x)))))
                                                              
                                                              (dolist (binding (reverse bindings))
                                                                (command-output "~vA : ~s" len (car binding) (cdr binding))))))))
                               (PRODUCTION-fired 
                                (let ((len 0))
                                  (let ((bindings (production-bindings (get-production prod))))
                                    (when bindings 
                                      (setf len (apply #'max (mapcar #'(lambda (x)
                                                                         (length (symbol-name (car x)))) bindings)))
                                      
                                      (dolist (binding (reverse bindings))
                                        (command-output "~vA : ~s" len (car binding) (cdr binding)))))))
                               (t nil)))
                 (declarative (case (evt-action *current-stepper-event*)
                                (retrieved-chunk (command-output "+retrieval>")
                                                 (pprint-chunk-spec 
                                                  (env-mod-last-dm-request env)))
                                (t nil)))
                 (t nil)))
              (t nil))))))
    

(defun tutor-completed (word)
  (if (cdr (assoc word *tutor-responses*))
      1 0))

(defun tutor-check (word binding)
  (if (equal binding (cdr (assoc word *tutor-bindings*)))
      (progn
        (setf (cdr (assoc word *tutor-responses*)) binding)
        1)
    0))

(defun tutor-answer (word)
  (cdr (assoc word *tutor-bindings*)))







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
