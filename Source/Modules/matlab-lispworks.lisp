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

(defpackage "MATLAB"
  (:use "COMMON-LISP" "FLI")
  (:export
   mx-add-field
   mx-array-to-string
   mx-calc-single-subscript
   mx-calloc
   mx-create-cell-array
   mx-create-cell-matrix
   mx-create-char-array
   mx-create-char-matrix-from-strings
   mx-create-double-matrix
   mx-create-double-scalar
   mat-open
   mat-close
   mat-get-dir
   mat-get-fp
   mat-get-variable
   mat-get-variable-info
   mat-get-next-variable
   mat-get-next-variable-info
   mat-delete-variable
   mat-put-variable
   mat-put-variable-as-global
   eng-open
   eng-open-single-use
   eng-close
   eng-eval-string
   eng-get-variable
   eng-put-variable
   eng-set-visible
   eng-get-visible
   eng-output-buffer))

(in-package "MATLAB")

;;; Should try and avoid hardcoding these paths in the future.


(register-module "/Applications/MATLAB_R2008b.app/bin/maci/libmat.dylib")
(register-module "/Applications/MATLAB_R2008b.app/bin/maci/libeng.dylib")




(define-c-typedef (file (:foreign-name "FILE")) :struct)

(define-c-typedef (mw-size (:foreign-name "mwSize")) :int)
(define-c-typedef (mw-index (:foreign-name "mwIndex")) :int)
(define-c-typedef (mw-signed-index (:foreign-name "mwSignedIndex")) :int)
(define-c-typedef (mx-array (:foreign-name "mxArray")) :struct)
(define-c-typedef (mx-char (:foreign-name "mxChar")) :uint16)
(define-c-typedef (mx-class-id (:foreign-name "mxClassID")) :enum)
(define-c-typedef (mx-complexity (:foreign-name "mxComplexity")) :enum)
(define-c-typedef (mat-file (:foreign-name "MATFile")) :struct)
(define-c-typedef (engine (:foreign-name "Engine")) :struct)

(define-foreign-function (mx-add-field "mxAddField" :source)
    ((mxarray-pointer (:pointer mx-array))
     (fieldname (:reference-pass :ef-mb-string :allow-null t)))
  :result-type :int
  :documentation "Add field to structure array")

(define-foreign-function (mx-array-to-string "mxArrayToString" :source)
    ((mxarray-pointer (:pointer mx-array)))
  :result-type (:pointer :char)
  :documentation "Convert array to string")

(define-foreign-function (mx-calc-single-subscript "mxCalcSingleSubscript" :source)
    ((mxarray-pointer (:pointer mx-array))
     (nsubs mw-size)
     (subs (:pointer mw-index)))
  :result-type mw-index
  :documentation "Offset from first element to desired element")

(define-foreign-function (mx-calloc "mxCalloc" :source)
    ((n mw-size)
     (size mw-size))
  :result-type (:pointer :void)
  :documentation "Allocate dynamic memory for array using MATLAB memory manager")

(define-foreign-function (mx-create-cell-array "mxCreateCellArray" :source)
    ((ndim mw-size)
     (dims (:pointer mw-size)))
  :result-type (:pointer mx-array)
  :documentation "Create unpopulated N-D cell mxArray")

(define-foreign-function (mx-create-cell-matrix "mxCreateCellMatrix" :source)
    ((m mw-size)
     (n mw-size))
  :result-type (:pointer mx-array)
  :documentation "Create unpopulated 2-D cell mxArray")

(define-foreign-function (mx-create-char-array "mxCreateCharArray" :source)
    ((m mw-size)
     (dims (:pointer mw-size)))
  :result-type (:pointer mx-array)
  :documentation "Create unpopulated N-D string mxArray")

(define-foreign-function (mx-create-char-matrix-from-strings "mxCreateCharMatrixFromStrings" :source)
    ((m mw-size)
     (str (:pointer (:pointer :char))))
  :result-type (:pointer mx-array)
  :documentation "Create populated 2-D string mxArray")

(define-foreign-function (mx-create-double-matrix "mxCreateDoubleMatrix" :source)
    ((m mw-size)
     (n mw-size)
     (complex-flag mx-complexity))
  :result-type (:pointer mx-array)
  :documentation "Create 2-D, double-precision, floating-point mxArray initialized to 0")

(define-foreign-function (mx-create-double-scalar "mx-create-double-scalar" :source)
    ((value :double))
  :result-type (:pointer mx-array)
  :documentation "Create scalar, double-precision array initialized to specified value")

(define-foreign-function (mat-open "matOpen" :source)
    ((filename (:reference-pass :ef-mb-string :allow-null t))
     (mode (:reference-pass :ef-mb-string :allow-null t)))
  :result-type (:pointer mat-file)
  :documentation "Open MAT-file")

(define-foreign-function (mat-close "matClose" :source)
    ((mat-file-pointer (:pointer mat-file)))
  :result-type :int
  :documentation "Close MAT-file")

(define-foreign-function (mat-get-dir "matGetDir" :source)
    ((mat-file-pointer (:pointer mat-file))
     (number (:reference-return :int)))
  :result-type (:pointer (:pointer :char))
  :documentation "Get directory of mxArrays in MAT-file")

(define-foreign-function (mat-get-fp "matGetFp" :source)
    ((mat-file-pointer (:pointer mat-file)))
  :result-type (:pointer file)
  :documentation "Get file pointer to MAT-file")

(define-foreign-function (mat-get-variable "matGetVariable" :source)
    ((mat-file-pointer (:pointer mat-file))
     (mxarray-name (:reference-pass :ef-mb-string :allow-null t)))
  :result-type (:pointer mx-array)
  :documentation "Read mxArray from MAT-files")

(define-foreign-function (mat-get-variable-info "matGetVariableInfo" :source)
    ((mat-file-pointer (:pointer mat-file))
     (mxarray-name (:reference-pass :ef-mb-string :allow-null t)))
  :result-type (:pointer mx-array)
  :documentation "Load array header information only")

(define-foreign-function (mat-get-next-variable "matGetNextVariable" :source)
    ((mat-file-pointer (:pointer mat-file))
     (mxarray-name (:pointer (:reference-pass :ef-mb-string :allow-null t))))
  :result-type (:pointer mx-array)
  :documentation "Read next mxArray from MAT-file")

(define-foreign-function (mat-get-next-variable-info "matGetNextVariableInfo" :source)
    ((mat-file-pointer (:pointer mat-file))
     (mxarray-name (:pointer (:reference-pass :ef-mb-string :allow-null t))))
  :result-type (:pointer mx-array)
  :documentation "Load array header information only")

(define-foreign-function (mat-delete-variable "matDeleteVariable" :source)
    ((mat-file-pointer (:pointer mat-file))
     (mxarray-name (:reference-pass :ef-mb-string :allow-null t)))
  :result-type :int
  :documentation "Delete named mxArray from MAT-file")

(define-foreign-function (mat-put-variable "matPutVariable" :source)
    ((mat-file-pointer (:pointer mat-file))
     (mxarray-name (:reference-pass :ef-mb-string :allow-null t))
     (mxarray-pointer (:pointer mx-array)))
  :result-type :int
  :documentation "Write mxArrays to MAT-files")

(define-foreign-function (mat-put-variable-as-global "matPutVariableAsGlobal" :source)
    ((mat-file-pointer (:pointer mat-file))
     (mxarray-name (:reference-pass :ef-mb-string :allow-null t))
     (mxarray-pointer (:pointer mx-array)))
  :result-type :int
  :documentation "Put mxArrays into MAT-files as originating from global workspace")

(define-foreign-function (eng-open "engOpen" :source)
    ((start-command (:reference-pass :ef-mb-string :allow-null t)))
  :result-type (:pointer engine)
  :documentation "Start MATLAB engine session")

(define-foreign-function (eng-open-single-use "engOpenSingleUse" :source)
    ((start-command (:reference-pass :ef-mb-string :allow-null t))
     (reserved :void :pointer);
     (return-status (:reference-return :int)))
  :result-type (:pointer engine)
  :documentation "Start MATLAB engine session for single, nonshared use")

(define-foreign-function (eng-close "engClose" :source)
    ((engine-pointer (:pointer engine)))
  :result-type :int
  :documentation "Quit MATLAB engine session")

(define-foreign-function (eng-eval-string "engEvalString" :source)
    ((engine-pointer (:pointer engine))
     (string (:reference-pass :ef-mb-string :allow-null t)));
  :result-type :int
  :documentation "Evaluate expression in string")

(define-foreign-function (eng-get-variable "engGetVariable" :source)
    ((engine-pointer (:pointer engine))
     (var-name (:reference-pass :ef-mb-string :allow-null t)))
  :result-type (:pointer mx-array)
  :documentation "Copy variable from MATLAB engine workspace")

(define-foreign-function (eng-put-variable "engPutVariable" :source)
    ((engine-pointer (:pointer engine))
     (var-name (:reference-pass :ef-mb-string :allow-null t))
     (array-pointer (:pointer mx-array)))
  :result-type :int)

(define-foreign-function (eng-set-visible "engSetVisible" :source)
    ((engine-pointer (:pointer engine))
     (value :boolean))
  :result-type :int
  :documentation "Show or hide MATLAB engine session")

(define-foreign-function (eng-get-visible "engGetVisible" :source)
    ((engine-pointer (:pointer engine))
     (value :boolean))
  :result-type :int
  :documentation "Determine visibility of MATLAB engine session")

(define-foreign-function (eng-output-buffer "engOutputBuffer" :source)
    ((engine-pointer (:pointer engine))
     (buffer (:reference-pass :ef-mb-string :allow-null t))
     (length :int))
  :result-type :int
  :documentation "Specify buffer for MATLAB output")
