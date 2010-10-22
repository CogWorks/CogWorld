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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (multiple-value-bind (out)
      (run-shell-command "sh -c 'sw_vers'"
                             :wait nil
                             :output :stream
                             :error-output nil)
    (with-open-stream (stream out)
        (do ((line (read-line stream nil)
                   (read-line stream nil)))
            ((null line))
          (cond
           ((starts-with line "ProductVersion:")
            (let ((ver (string-left-trim '(#\Space #\Tab #\Newline) (trim-prefix "ProductVersion:" line))))
              (cond
               ((starts-with ver "10.4")
                (pushnew :macosx-tiger *features*))
               ((starts-with ver "10.5")
                (pushnew :macosx-leopard *features*))
               ((starts-with ver "10.6")
                (pushnew :macosx-snow-leopard *features*))))))))))
