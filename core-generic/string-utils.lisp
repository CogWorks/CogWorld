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

(defpackage "STRING-UTILS"
  (:use "COMMON-LISP")
  (:export
   starts-with
   trim-prefix))

(in-package "STRING-UTILS")

(defun trim-left-spaces (string)
  "Returns a substring of string, with all Tab and Space characters stripped
   off the beginning."
  (string-left-trim '(#\Tab #\Space) string))

(defun starts-with (string prefix &optional ignore-left-whitespace-p)
  "Returns T if the designed string starts with the desired string prefix."
  (when ignore-left-whitespace-p (setf string (trim-left-spaces string)))
  (unless (< (length string) (length prefix))
    (loop for i from 0 below (length prefix)
	  unless (char= (char string i) (char prefix i))
	  do (return nil) finally (return T))))

(defun trim-prefix (prefix string &key (replace-prefix nil))
  "Returns a new string that does not contain prefix anymore. Left white spaces
   will be ignored but kept. Prefix will be replace by as many space characters
   than its length if replace-prefix is T."
  (let ((start (or (position (char prefix 0) string :test #'char=) 0)))
    (concatenate 'string
		 (subseq string 0 start)
		 (if replace-prefix
		     (make-string (length prefix) :initial-element #\Space)
		     "")
		 (subseq string (+ start (length prefix))))))
