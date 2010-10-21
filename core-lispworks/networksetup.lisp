#|=============================================================================
 Copyright (C) 2010 Ryan Hope <hoper2@rpi.edu>
 Copyright (C) 2010 CogWorks Lab <http://cogworks.cogsci.rpi.edu>

 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 =============================================================================|#

(defpackage "NETWORKSETUP"
  (:nicknames "NS")
  (:use "COMMON-LISP" "MAC-FEATURES" "STRING-UTILS")
  (:export
   set-airport-power
   list-all-hardware-ports
   device-from-alias))

(in-package "NETWORKSETUP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NetworkSetup Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+MACOSX-LEOPARD
(defun set-airport-power (state)
  (if state
      (sys:call-system "sudo networksetup -setairportpower on")
    (sys:call-system "sudo networksetup -setairportpower ~a off")))

#+MACOSX-SNOW-LEOPARD
(defun set-airport-power (device state)
  (let (cmd)
    (if state
        (setf cmd (format nil "sudo networksetup -setairportpower ~a on" device))
      (setf cmd (format nil "sudo networksetup -setairportpower ~a off" device)))
    (sys:call-system cmd)))

(defun list-all-hardware-ports ()
  "Returns a list of hash tables containing the alias, device name, and MAC
   address for all hardware ports."
  (multiple-value-bind (out)
      (sys:run-shell-command "sh -c 'networksetup -listallhardwareports'"
                             :wait nil
                             :output :stream
                             :error-output nil)
    (let ((result-hash (make-hash-table :test 'equal))
          (tmp-hash (make-hash-table :test 'equal))
          (alias nil))
      (with-open-stream (stream out)
        (do ((line (read-line stream nil)
                   (read-line stream nil)))
            ((null line))
          (cond
           ((starts-with line "Hardware Port: ")
            (setf alias (trim-prefix "Hardware Port: " line)))
           ((starts-with line "Device: ")
            (setf (gethash "device" tmp-hash) (trim-prefix "Device: " line)))
           ((starts-with line "Ethernet Address: ")
            (setf (gethash "mac" tmp-hash) (trim-prefix "Ethernet Address: " line))
            (setf (gethash alias result-hash) tmp-hash)
            (setf tmp-hash (make-hash-table :test 'equal))))))
      result-hash)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun device-from-alias (alias)
  "Returns the device name from a device alias"
  (let ((hardware-ports (list-all-hardware-ports)))
    (let ((device (gethash "device" (gethash alias hardware-ports))))
      device)))
