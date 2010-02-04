
;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2009 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : key-press-module.lisp
;;; Version     : 0.9a1
;;; 
;;; Description : Module which extends the motor module with 2 additional
;;;             : actions for the manual buffer isa hold-key and isa release-key.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Add some real documentation...
;;; 
;;; ----- History -----
;;;
;;; 2009.03.19 Dan
;;;             : * Moved it from a hack in some other code to its own file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)


(defun create-key-press-module (model-name)
  (declare (ignore model-name))
  (make-hash-table :size 12 :test #'equalp))

(defun reset-key-press-module (module)
  (clrhash module))


(define-module :key-press nil nil
   :version "0.9a1"
   :documentation "Simple module for tracking keys (just fingers right now) being held down"
   :creation create-key-press-module
   :reset reset-key-press-module 
)

(defstyle press-and-hold-key punch hand finger)

(defmethod compute-exec-time ((mtr-mod motor-module) (self press-and-hold-key))
  (+ (init-time mtr-mod) (key-closure-time (current-device-interface))))


(defmethod compute-finish-time ((mtr-mod motor-module) (self press-and-hold-key))
  (* 2 (init-time mtr-mod)))


(defmethod queue-output-events ((mtr-mod motor-module) (self press-and-hold-key))
  (schedule-event-relative 0 'hold-key :destination :key-press :module :motor 
                           :output 'medium  
                           :params (list (hand self) (finger self))))

(defstyle release-held-key press-and-hold-key hand finger)

(defmethod queue-output-events ((mtr-mod motor-module) (self release-held-key))
  (schedule-event-relative 0 'release-key :destination :key-press :module :motor 
                           :output 'medium  
                           :params (list (hand self) (finger self))))

(defun hold-key (module hand finger)
  (setf (gethash (list hand finger) module) t)
  (device-hold-finger (current-device) hand finger))


(defun release-key (module hand finger)
  (setf (gethash (list hand finger) module) nil)
  (device-release-finger (current-device) hand finger))


(defun handle-hold-request (motor chunk-spec)
  (declare (ignore motor))
  (let* ((hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
                   (verify-single-explicit-value 
                    (chunk-spec-slot-spec chunk-spec 'hand) 
                    :motor 'hold-key 'hand)
                 nil))
         (finger (if (slot-in-chunk-spec-p chunk-spec 'finger)
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'finger)
                      :motor 'hold-key 'finger)
                   nil)))
    
    (when (and hand finger)
      (if (null (gethash (list hand finger) (get-module :key-press)))
          (schedule-event-relative 
           0 
           'press-and-hold-key
           :destination :motor
           :params (list :hand hand :finger finger)
           :module :motor
           :output 'low)
        (print-warning "The ~S ~S finger is already being held.  No action taken."  hand finger)))))

(defun handle-release-request (motor chunk-spec)
  (declare (ignore motor))
  (let* ((hand (if (slot-in-chunk-spec-p chunk-spec 'hand) 
                   (verify-single-explicit-value 
                    (chunk-spec-slot-spec chunk-spec 'hand) 
                    :motor 'hold-key 'hand)
                 nil))
         (finger (if (slot-in-chunk-spec-p chunk-spec 'finger)
                     (verify-single-explicit-value 
                      (chunk-spec-slot-spec chunk-spec 'finger)
                      :motor 'hold-key 'finger)
                   nil)))
    
    (when (and hand finger)
      (if (gethash (list hand finger) (get-module :key-press))
          (schedule-event-relative 
           0 
           'release-held-key
           :destination :motor
           :params (list :hand hand :finger finger)
           :module :motor
           :output 'low)
        (print-warning "The ~S ~S finger is not being held.  No action taken."  hand finger)))))

(extend-manual-requests (hold-key hand finger) handle-hold-request)
(extend-manual-requests (release-key hand finger) handle-release-request)

#+:lispworks
(defmethod device-hold-finger ((dev capi:interface) hand finger)
  (let* ((kb (keyboard (current-device-interface)))
         (mot-mod (get-module :motor))
         (hnd (case hand (right (right-hand mot-mod)) (left (left-hand mot-mod))))
         (loc-hnd (loc hnd))
         (loc-finger (second (find finger (slot-value hnd 'finger-offsets)  :key 'first)))
         (loc (vector (+ (svref loc-hnd 0) (svref loc-finger 0)) (+ (svref loc-hnd 1) (svref loc-finger 1))))
         (key (loc->key kb loc))
         (key-code (gethash (char-code key) keycodes)))
    (cgpostkeyboardevent 1 key-code t)
    ))
#+:lispworks
(defmethod device-release-finger ((dev capi:interface) hand finger)
  (let* ((kb (keyboard (current-device-interface)))
         (mot-mod (get-module :motor))
         (hnd (case hand (right (right-hand mot-mod)) (left (left-hand mot-mod))))
         (loc-hnd (loc hnd))
         (loc-finger (second (find finger (slot-value hnd 'finger-offsets)  :key 'first)))
         (loc (vector (+ (svref loc-hnd 0) (svref loc-finger 0)) (+ (svref loc-hnd 1) (svref loc-finger 1))))
         (key (loc->key kb loc))
         (key-code (gethash (char-code key) keycodes)))
    (cgpostkeyboardevent 1 key-code nil)))




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