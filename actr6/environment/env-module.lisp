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
;;; Filename    : env-module.lisp (was hook-fns.lisp)
;;; Version     : 2.0
;;; 
;;; Description : NO system dependent code.
;;;             : This file contains the functions and variables 
;;;             : for managing the handlers connected to the hook functions 
;;;             : as well as the definition of the module which coordinates
;;;             : the operations now.
;;; Bugs        : 
;;; 
;;; Todo        : [ ] Move all the stepper control globals into the module
;;;             :     instance.
;;; 
;;; ----- History -----
;;;
;;; 05/10/2002  Dan
;;;             : Added this header
;;; 09/25/2002  Dan
;;;             : Updated with the newest ACT-R code (v5.0.6)
;;; 10/01/2002  Dan
;;;             : Updated version to 1.1 and fixed the packaging
;;;             : for building a standalone in ACL.
;;; 04/22/2004  Dan
;;;             : Updated the master-process class to version 2.2
;;;             : and added the LGPL license stuff.
;;; 4/22/2004   Dan [1.5]
;;;             : Added the license info.
;;; ------------------------------------------------------------------------
;;; 2005.04.12  Dan [2.0]
;;;             : * Moving to ACT-R 6.
;;;             : * Get rid of all the ACT-R 5 function redefinitions.
;;;             : * Using a module to handle things.  That gives me some
;;;             :   encapsulation that it didn't have before (an actual
;;;             :   CLOS class with some class slots would be even better
;;;             :   but not for the first pass) and lends itself to the 
;;;             :   eventual support of multiple models.
;;; 2005.04.20  Dan
;;;             : * Changed create-environment-module to put #' before the
;;;             :   lambdas in the add-... for Lispworks.
;;; 2007.06.26  Dan
;;;             : * Fixed an annoying bug with the warning about multiple
;;;             :   model support getting "stuck" if a model broke on
;;;             :   loading.
;;; 2007.08.03  Dan
;;;             : * Moved the *environment-sockets* defvar here to avoid a
;;;             :   compiler warning.
;;; 2007.08.08  Dan
;;;             : * Updated the reset function so that it clears the run until
;;;             :   conditions for the stepper.
;;; 2007.08.13  Dan
;;;             : * Adding the reset-hook-list processing so handlers can
;;;             :   be updated on a reset.
;;; 2007.08.15  Dan
;;;             : * Created named functions for the declarative and procedural
;;;             :   hooks that the env sets to make the sgp output cleaner.
;;;             : * Added a slot for the esc parameter and a monitoring of it
;;;             :   so the enviroment can check it more easily.
;;; 2008.05.16  Dan
;;;             : * Added the conflict-nil-hook-list slot to support controls
;;;             :   which need to retrun something from the hook function,
;;;             :   but don't want that passed back to the conflict set.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

;(in-package :cl-user)

;#+(and :allegro-ide (not :ACTR-ENV-ALONE)) (in-package :cg-user)


(defvar *creation-hook-list* nil)
(defvar *deletion-hook-list* nil)
(defvar *reset-hook-list* nil)

;;; a list of open sockets and the processes that are handling them.

(defvar *environment-sockets* nil)

(defstruct (environment-module (:conc-name env-mod-))
  esc
  model-name
  pre-hook-list
  post-hook-list
  conflict-hook-list
  conflict-nil-hook-list
  pre-hook
  post-hook                             
  conflict-set
  last-dm-request
  last-dm-set)

(defvar *stepper-skip-type* nil)
(defvar *stepper-skip-val* nil)

(defun create-environment-module (model-name) 
  (when *environment-sockets* 
    (if (> (length (mp-models)) 1)
        (print-warning "Cannot (yet) use the environment with multiple models!")
      (let ((module (make-environment-module :model-name model-name)))
        (setf (env-mod-pre-hook module)
          (add-pre-event-hook #'(lambda (event)
                                (dolist (x (env-mod-pre-hook-list module))
                                  (update-handler x event)))))
        (setf (env-mod-post-hook module)
          (add-post-event-hook #'(lambda (event)
                                 (dolist (x (env-mod-post-hook-list module))
                                   (update-handler x event)))))
        (dolist (x *creation-hook-list*)
          (update-handler x model-name))
        module))))


(defun reset-environment-module (env-mod)
  (when env-mod
    (setf (env-mod-conflict-set env-mod) nil)
    (setf (env-mod-last-dm-request env-mod) nil)
    (setf (env-mod-last-dm-set env-mod) nil)
    
    (setf *stepper-skip-type* nil)
    (setf *stepper-skip-val* nil)
    
    (when *environment-sockets*
    
    ;; Push the commands onto the appropriate hooks
        
      (sgp :conflict-set-hook env-cs-hook
           :retrieval-request-hook env-rr-hook
           :retrieval-set-hook env-rs-hook)
      
      (dolist (x *reset-hook-list*)
        (update-handler x x)))))
    


(defun env-cs-hook (cs)
  (let ((env-mod (get-module :environment)))
    (when env-mod
      (setf (env-mod-conflict-set env-mod)
        (copy-list cs))
      (let ((val nil))
        ;; First update the ones that ignore return values
        
        (dolist (x (env-mod-conflict-nil-hook-list env-mod) val)
          (update-handler x cs))
      
      ;; Then do the ones that might return something
        
        (dolist (x (env-mod-conflict-hook-list env-mod) val)
          (let ((r (update-handler x cs)))
            (when r
              (setf val r))))))))
    
(defun env-rr-hook (request)
  (let ((env-mod (get-module :environment)))
    (when env-mod
      (setf (env-mod-last-dm-request env-mod)
        request)))
  nil)

(defun env-rs-hook (ret-set)
  (let ((env-mod (get-module :environment)))
    (when env-mod
      (setf (env-mod-last-dm-set env-mod)
        (copy-list ret-set))))
  nil)


(defun delete-environment-module (env-mod)
  (when env-mod
    (delete-event-hook (env-mod-pre-hook env-mod))
    (delete-event-hook (env-mod-post-hook env-mod))
    (dolist (x *deletion-hook-list*)
          (update-handler x nil))))


(defun environment-params (env-mod param)
  (when env-mod
    (setf (env-mod-esc env-mod) (cdr param))))

(define-module-fct :environment nil 
  (list (define-parameter :esc :owner nil))
  :params 'environment-params
  :creation 'create-environment-module
  :reset '(nil reset-environment-module)
  :version #+:ACTR-ENV-ALONE "2.0-s"
           #-:ACTR-ENV-ALONE "2.0"
  
  :documentation "A module to handle the environment connection if opened"
  :delete 'delete-environment-module)


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
