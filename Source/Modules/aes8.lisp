;;;; The AES (Rijndael) crypto algorithm in Common Lisp
;;;  Encrypts 128 bits blocks, and works with 128, 192, and 256
;;;  bits keys
;;;
;;;; Lisense: LGPL
;;;
;;;; Copyright: Jørn Inge Vestgården, 2004
;;;
;;;; Version 0.0 pre alfa
;;;
;;;; This implementation is based on the C reference implementation
;;; written by Paulo Barreto and Vincent Rijman. Note that while the
;;; reference implementation handles blocks of 128, 192, and 256 bits,
;;; the AES standard's (and this implementation's) blocksize is 128 bits
;;;
;;;; Status: seems to work
;;;
;;;; Speed: generting the Nist vectors takes about 50% longer with CMU CL
;;; commared to reference C implementation (that is slow)
;;; For high speed requirements i suggest linking with an optimized C
;;; or assembler implementation
;;;
;;;; TODO:
;;   - make a more traditional interface
;;   - remove NITS test stuff
;;   - more testing (on other CLs than CMU), clean up,
;;;        

(defpackage "AES8"
  (:use "COMMON-LISP")
  (:export "MAKE-CRYPTO" "+BLOCK-BITS+"
	   "ECB-VT-KAT"
	   "ECB-VK-KAT"
	   "ECB-MCT"
	   "CBC-MCT"
	   "MAKE-VT-KATS"
	   "MAKE-VK-KATS"
	   "MAKE-ECB-ENCRYPT-MCTS"
	   "MAKE-ECB-DECRYPT-MCTS"
	   "MAKE-CBC-ENCRYPT-MCTS"
	   "MAKE-CBC-DECRYPT-MCTS"
	   "MAKE-ALL-TESTS"
	   "RIN->ID"))


(in-package :aes8)


;;;; Constants


(defconstant +maxrounds+ 14 "Maximum number of rounds")

(defconstant +maxkc+ 8 "Max number of key blocks")

(defconstant +bc+ 4 "Number 32-bits blocks inside the 128-bits block")

(defconstant +block-bits+ 128 "Number of  block bits")

(defconstant +block-bytes+ 16)

(defconstant +block-dim+ (list 4 +bc+)
  "the dimensions of the blocks swallowed by encrypt and decrypt")

(defconstant +key-block-dim+ (list 4 8))


;;;; Macros and definitions

(declaim (optimize (speed 3)
		     (compilation-speed 0)
		     (debug 1)
		     (safety 1)
		     (space 0)))


(declaim (inline MUL MIX-COLUMNS INV-MIX-COLUMNS 
		 ADD-ROUND-KEY SHIFT-ROWS INV-SHIFT-ROWS
		 SUBSTITUTION))


(defmacro for (var start stop &body body)
  "A for-loop. Works for fixnums only"
  (let ((gstop (gensym)))
    `(declare (fixnum ,var ,gstop))
    `(do ((,var ,start (the fixnum (1+ ,var)))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
    ,@body)))


(defmacro xor-on (x y)
  "applies logxor on the arguments and puts resutl in first argument"  
  `(setf ,x (the fixnum (logxor  (the fixnum ,x) (the fixnum ,y)))))


;;;; tables


(defconstant +log-table+
 #256(  0   0  25   1  50   2  26 198  75 199  27 104  51 238 223   3
      100   4 224  14  52 141 129 239  76 113   8 200 248 105  28 193
      125 194  29 181 249 185  39 106  77 228 166 114 154 201   9 120 
      101  47 138   5  33  15 225  36  18 240 130  69  53 147 218 142
      150 143 219 189  54 208 206 148  19  92 210 241  64  70 131  56
      102 221 253  48 191   6 139  98 179  37 226 152  34 136 145  16
      126 110  72 195 163 182  30  66  58 107  40  84 250 133  61 186
       43 121  10  21 155 159  94 202  78 212 172 229 243 115 167  87
      175  88 168  80 244 234 214 116  79 174 233 213 231 230 173 232
       44 215 117 122 235  22  11 245  89 203  95 176 156 169  81 160
      127  12 246 111  23 196  73 236 216  67  31  45 164 118 123 183
      204 187  62  90 251  96 177 134  59  82 161 108 170  85  41 157
      151 178 135 144  97 190 220 252 188 149 207 205  55  63  91 209
       83  57 132  60  65 162 109  71  20  42 158  93  86 242 211 171
       68  17 146 217  35  32  46 137 180 124 184  38 119 153 227 165
      103  74 237 222 197  49 254  24  13  99 140 128 192 247 112   7)
  "Multiplication in GF(2^8) lookup table.")

(defconstant +alog-table+
    #256( 1   3   5  15  17  51  85 255  26  46 114 150 161 248  19  53
      95 225  56  72 216 115 149 164 247   2   6  10  30  34 102 170
     229  52  92 228  55  89 235  38 106 190 217 112 144 171 230  49
      83 245   4  12  20  60  68 204  79 209 104 184 211 110 178 205
      76 212 103 169 224  59  77 215  98 166 241   8  24  40 120 136
     131 158 185 208 107 189 220 127 129 152 179 206  73 219 118 154
     181 196  87 249  16  48  80 240  11  29  39 105 187 214  97 163
     254  25  43 125 135 146 173 236  47 113 147 174 233  32  96 160
     251  22  58  78 210 109 183 194  93 231  50  86 250  21  63  65
     195  94 226  61  71 201  64 192  91 237  44 116 156 191 218 117
     159 186 213 100 172 239  42 126 130 157 188 223 122 142 137 128
     155 182 193  88 232  35 101 175 234  37 111 177 200  67 197  84
     252  31  33  99 165 244   7   9  27  45 119 153 176 203  70 202
      69 207  74 222 121 139 134 145 168 227  62  66 198  81 243  14
      18  54  90 238  41 123 141 140 143 138 133 148 167 242  13  23
      57  75 221 124 132 151 162 253  28  36 108 180 199  82 246   1)
  "Multiplication in GF(2^8) lookup table.")

(defconstant +s-table+
    #256( 99 124 119 123 242 107 111 197  48   1 103  43 254 215 171 118
      202 130 201 125 250  89  71 240 173 212 162 175 156 164 114 192
      183 253 147  38  54  63 247 204  52 165 229 241 113 216  49  21
        4 199  35 195  24 150   5 154   7  18 128 226 235  39 178 117
        9 131  44  26  27 110  90 160  82  59 214 179  41 227  47 132
       83 209   0 237  32 252 177  91 106 203 190  57  74  76  88 207
      208 239 170 251  67  77  51 133  69 249   2 127  80  60 159 168
       81 163  64 143 146 157  56 245 188 182 218  33  16 255 243 210
      205  12  19 236  95 151  68  23 196 167 126  61 100  93  25 115
       96 129  79 220  34  42 144 136  70 238 184  20 222  94  11 219
      224  50  58  10  73   6  36  92 194 211 172  98 145 149 228 121
      231 200  55 109 141 213  78 169 108  86 244 234 101 122 174   8
      186 120  37  46  28 166 180 198 232 221 116  31  75 189 139 138
      112  62 181 102  72   3 246  14  97  53  87 185 134 193  29 158
      225 248 152  17 105 217 142 148 155  30 135 233 206  85  40 223
      140 161 137  13 191 230  66 104  65 153  45  15 176  84 187  22)
  "Rijndael S-box.")

(defconstant +si-table+
    #256( 82   9 106 213  48  54 165  56 191  64 163 158 129 243 215 251
      124 227  57 130 155  47 255 135  52 142  67  68 196 222 233 203
       84 123 148  50 166 194  35  61 238  76 149  11  66 250 195  78
        8  46 161 102  40 217  36 178 118  91 162  73 109 139 209  37
      114 248 246 100 134 104 152  22 212 164  92 204  93 101 182 146
      108 112  72  80 253 237 185 218  94  21  70  87 167 141 157 132
      144 216 171   0 140 188 211  10 247 228  88   5 184 179  69   6
      208  44  30 143 202  63  15   2 193 175 189   3   1  19 138 107
       58 145  17  65  79 103 220 234 151 242 207 206 240 180 230 115
      150 172 116  34 231 173  53 133 226 249  55 232  28 117 223 110
       71 241  26 113  29  41 197 137 111 183  98  14 170  24 190  27
      252  86  62  75 198 210 121  32 154 219 192 254 120 205  90 244
       31 221 168  51 136   7 199  49 177  18  16  89  39 128 236  95
       96  81 127 169  25 181  74  13  45 229 122 159 147 201 156 239
      160 224  59  77 174  42 245 176 200 235 187  60 131  83 153  97
       23  43   4 126 186 119 214  38 225 105  20  99  85  33  12 125)
  "Rijndael inverted S-box.")

(defconstant +rcon-table+
  #(#X01 #X02 #X04 #X08 #X10 #X20 #X40 #X80 #X1b #X36 #X6c #Xd8
    #Xab #X4d #X9a #X2f #X5e #Xbc #X63 #Xc6 #X97 #X35 #X6a #Xd4
	 #Xb3 #X7d #Xfa #Xef #Xc5 #X91 ))


;;;; Help Functions


(defun make-plain-block ()
  "makes a 16 element block"
  (make-array +block-bytes+ :initial-element 0 :element-type 'unsigned-byte))

(defun make-key-block ()
  "makes a x8 block"
  (make-array +key-block-dim+ :element-type 'unsigned-byte
	      :initial-element 0))

(defun make-block ()
  "makes an 4x4 block"
  (make-array +block-dim+ :initial-element 0 :element-type 'unsigned-byte))

(defun hex-str->bin-array (hex-str)
  "converts a hex string to binary array. Length of
hex string must be mulitple of 2"
  (let* ((bin-len (/ (length hex-str) 2))
	 (bin (make-array bin-len :element-type 'unsigned-byte)))
    (dotimes (i bin-len)
      (setf (aref bin i)
	    (parse-integer hex-str :radix 16
			   :start (* 2 i)
			   :end (* 2 (1+ i)))))   
    bin))

(defun bin-array->hex-str (bin)
  (let ((hex (make-string (* 2 (length bin)))))
    (dotimes (i (length bin))
      (let ((h (format nil "~2,'0X" (aref bin i))))	
	(setf (char hex (* 2 i)) (char h 0))
	(setf (char hex (1+ (* 2 i))) (char h 1))))
    hex))


(defun make-key-schedule (k-array)
  "makes a key schedule from key material provided"
  ;; TODO: simplify and clean up
  (let* ((Key-bits (* (length k-array) 8))
	 (kc (/ key-bits 32))
	 (rounds (+ 6 (/ key-bits 32)))
	 (tk (make-key-block))
	 (n 0)
	 (rconpointer 0)
	 (max-n (* (1+ rounds) +bc+))
	 (w (make-array (1+ +maxrounds+) :initial-element nil)))
    (dotimes (i (1+ +maxrounds+))
      (setf (aref w i) (make-key-block)))
    (dotimes (j kc)
      (dotimes (i 4)
	(setf (aref tk i j) (aref k-array (+ (* 4 j) i)))))
    (setf n 0) ; TODO: remove this
    (do ((j 0 (+ j 1)))
	((or (>= j kc) (>= n max-n)))
      (dotimes (i 4)
	(let ((tmp (aref w (floor n +bc+))))
	  (setf (aref  tmp i (mod n +bc+))
		(aref tk i j))))
      (incf n))
    
    (do () ((>= n max-n))
      (dotimes (i 4)
	(xor-on (aref tk i 0)
		(aref +s-table+ (aref tk (mod (+ i 1) 4) (- kc 1)))))
      (xor-on (aref tk 0 0) 
	      (aref +rcon-table+ rconpointer))
      (incf rconpointer)
      (if (/= kc 8)
	  (for j 1 (1- kc)
	       (dotimes (i 4)
		 (xor-on (aref tk i j) (aref tk i (1- j)))))
	(progn
	  (for j 1 (1- (floor kc 2))
	       (dotimes (i 4)
		 (xor-on (aref tk i j) (aref tk i (1- j)))))
	  (dotimes (i 4)
	    (xor-on (aref tk i (floor kc 2))
		    (aref +s-table+ (aref tk i (1- (floor kc 2))))))
	  (for j (1+ (floor kc 2)) (1- kc)
	       (dotimes (i 4)
		 (xor-on (aref tk i j) (aref tk i (1- j)))))))
      (do ((j 0 (+ j 1)))
	  ((or (>= j kc) (>= n max-n)))
	(dotimes (i 4)
	  (let ((tmp  (aref w (floor n +bc+))))
	    (setf (aref tmp i (mod n +bc+))
		  (aref tk i j))))
	(incf n)))
    w))


(defun mul (a b)
  "The GF(2^m) multiplication"
  ;; If somethings shall be optimized, it's this one
  (declare (fixnum a b))
  (the fixnum
    (if (or (zerop a) (zerop b))
	0
      (svref +alog-table+
	     (mod (the fixnum (+ (the fixnum (svref +log-table+ a))
				 (the fixnum (svref +log-table+ b)))) 255)))))

       
(defun add-round-key (state rk-elm)
  "Xor corresponding text input and round key input bytes"
  (declare ((simple-array unsigned-byte (4 4)) state))
  (declare ((simple-array unsigned-byte (4 8)) rk-elm))
  (for i 0 3
       (for j 0 (1- +bc+)
	    (xor-on (aref state i j)  (aref rk-elm i j)))))


(defun shift-rows (state tmp)
  "Shift rows. Row no 0 is left unshifted"
  (declare ((simple-array unsigned-byte (4 4)) state tmp))
  (for i 1 3
       (for j 0 (1- +bc+)
	    (setf (aref tmp j 0)
		  (aref state i (mod (+ i j) 4))))
       (for j 0 (1- +bc+)
	    (setf (aref state i j) (aref tmp j 0)))))


(defun inv-shift-rows (state tmp)
  "Inverse of shift rows"
  (declare ((simple-array unsigned-byte (4 4)) state tmp))
  (for i 1 3
       (for j 0 (1- +bc+)
	    (setf (aref tmp j 0)
		  (aref state i (mod (- j i) 4))))
       (for j 0 (1- +bc+)
	    (setf (aref state i j) (aref tmp j 0)))))


(defun substitution (state box)
  "Replaces the byts of the input with the values of the box"
  (declare ((simple-array unsigned-byte (4 4)) state))
  (declare (type (simple-vector 256) box))
  (for i 0 3
       (for j 0 (1- +bc+)
	    (setf (aref state i j) (svref box (aref state i j))))))

       
(defun mix-columns (state tmp)
  "Mix the four bytes of a column in a linear way"
  (declare ((simple-array unsigned-byte (4 4)) state))
  (declare ((simple-array unsigned-byte (4 4)) tmp))	
  (for j 0 (1- +bc+)
       (for i 0 3
	    (setf (aref tmp i j)
		  (logxor
		   (mul 2 (aref state i j))
		   (mul 3 (aref state (mod (1+ i) 4) j))
		   (the fixnum (aref state (mod (+ i 2) 4) j))
		   (the fixnum (aref state (mod (+ i 3) 4) j))))))
  (dotimes (i 4)
    (dotimes (j +bc+)
      (setf (aref state i j) (aref tmp i j)))))


(defun inv-mix-columns (state tmp)
  "Inverse of mix columns"
  (declare ((simple-array unsigned-byte (4 4)) state tmp))
  (for j 0 (1- +bc+)
       (for i 0 3
	    (setf (aref tmp i j)
		  (logxor
		   (mul #Xe (aref state i j))
		   (mul #Xb (aref state (mod (+ i 1) 4) j))
		   (mul #Xd (aref state (mod (+ i 2) 4) j))
		   (mul #X9 (aref state (mod (+ i 3) 4) j))))))
  (dotimes (i 4)
    (dotimes (j +bc+)
      (setf (aref state i j) (aref tmp i j)))))
       

;;;; The encryption/decryption functions


(defun rijndael-encrypt (state key-schedule key-length tmp)
  "the basic rijndael encryption box"
  (declare ((simple-array unsigned-byte (4 4)) state tmp))
  (declare ((array (simple-array unsigned-byte (4 8)) *) key-schedule))
  (let ((rounds (+ 6 (/ key-length 32))))
    (add-round-key state (aref key-schedule 0))
    (for r 1 (1- rounds)
	 (substitution state +s-table+)
	 (shift-rows state tmp)
	 (mix-columns state tmp)
	 (add-round-key state (aref key-schedule r)))    
    (substitution state +s-table+)
    (shift-rows state tmp)
    (add-round-key state (aref key-schedule rounds))))


(defun rijndael-decrypt (state key-schedule key-length tmp)
  "the basic rijndael decryption box"	
  (declare ((simple-array unsigned-byte (4 4)) state tmp))
  (declare ((array (simple-array unsigned-byte (4 8)) *) key-schedule))
  (let ((rounds (+ 6 (/ key-length 32))))
    (add-round-key state (aref key-schedule rounds))
    (substitution state +si-table+)
    (inv-shift-rows state tmp)
    (loop for r downfrom (1- rounds) above 0 do
	  (add-round-key state (aref key-schedule r))
	  (inv-mix-columns state tmp)
	  (substitution state +si-table+)
	  (inv-shift-rows state tmp))
    (add-round-key state (aref key-schedule 0))))


(defun make-crypto (key-mat
		    direction
		    &key
		    (mode :ecb)
		    (iv "00000000000000000000000000000000"))
  "Factory making a encryption/decryption function
Input: key-mat is 128, 192, or 256 bits hexadesimal number
       direction is :encrypt or :decrypt
       mode is :ecb or :cbc
       iv is 128 bits hexadesimal number
Ouput: a function f: f in &optional (out (make-plain-block)),
       where in is an array of at least 16 byts. Returns out."
  (declare (string key-mat) (string iv))
  (unless (member (length key-mat) '(32 48 64))
    (error "Length key-mat must be 128, 192, or 256 bits"))
  (let ((state (make-block))
	(tmp (make-block))
	(key-length (* 4 (length key-mat)))
	(key-schedule (make-key-schedule (hex-str->bin-array key-mat))))
  (labels
    (     
     (encrypt-block (in out)
       (declare ((simple-array unsigned-byte (4 4)) state))
       (declare ((array (simple-array unsigned-byte (4 8)) *) key-schedule))
       (dotimes (i 4)
	 (dotimes (j +bc+)
	   (setf (aref state j i)
		 (aref in (+ (* i 4) j)))))
       (rijndael-encrypt state key-schedule key-length tmp)
       (dotimes (i 4)
	 (dotimes (j +bc+)
	   (setf (aref out (+ (* i 4) j))
		 (aref state j i))))
       out)

     (decrypt-block (in out)
       (declare ((simple-array unsigned-byte (4 4)) state))
       (declare ((array (simple-array unsigned-byte (4 8)) *) key-schedule))
       (dotimes (i 4)
	 (dotimes (j +bc+)
	   (setf (aref state j i)
		 (aref in (+ (* i 4) j)))))
       (rijndael-decrypt state key-schedule key-length tmp)
       (dotimes (i 4)
	 (dotimes (j +bc+)
	   (setf (aref out (+ (* i 4) j))
		 (aref state j i))))
       out)
     
     (make-ecb-encrypt ()
	#'(lambda (in &optional (out (make-plain-block))) (encrypt-block in out)))

     (make-ecb-decrypt ()
        #'(lambda (in &optional (out (make-plain-block))) (decrypt-block in out)))

     (make-cbc-encrypt (iv)
       (let ((iv-in (hex-str->bin-array (subseq iv 0 32))))
	 #'(lambda (buf &optional (out  (make-plain-block)))
	     (map-into iv-in #'logxor iv-in buf)
	     (encrypt-block iv-in out)
	     (replace iv-in out)
	     out)))
      
     (make-cbc-decrypt (iv)
        (let ((iv-out (hex-str->bin-array (subseq iv 0 32))))
	  #'(lambda (buf &optional (out (make-plain-block)))
	      (decrypt-block buf out)
	      (map-into out #'logxor out iv-out)
	      (replace iv-out buf)	      	     
	      out)))
     ) 
    
    (case direction
      (:encrypt
       (case mode
	 (:ecb
	  (make-ecb-encrypt))
	 (:cbc
	  (make-cbc-encrypt iv))
	 (t
	  (error "Uknown encryption mode: ~A" mode))))
      (:decrypt
       (case mode
	 (:ecb
	  (make-ecb-decrypt))
	 (:cbc
	  (make-cbc-decrypt iv))
	 (t
	  (error "Uknown decryption mode: ~A" mode))))
      (t
       (error "Unknown direction. Must be :encrypt or :decrypt"))))))




;; Development test-stuff


(defun test-ecb ()
  (let* ((e (make-crypto "0123456789abcde10123456789abcde1"
			 :encrypt))
	 (d (make-crypto "0123456789abcde10123456789abcde1"
			 :decrypt))
	 (plain #1a(0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 ))
	 (crypt (funcall e plain))
	 (plain2 (funcall d crypt)))
    (format t "plain:  ~A~%" plain)
    (format t "crypt:  ~A~%" crypt)
    (format t "plain2: ~A~%" plain2)))


(defun test-many (num)
  (let ((e (make-crypto "0123456789abcde10123456789abcde1"
			:encrypt))
	(d (make-crypto "0123456789abcde10123456789abcde1"
			:decrypt))
	(buf #1a(0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 ))
	(out (make-array 32 :element-type 'unsigned-byte)))
    (dotimes (i num)
      (funcall e buf out)
      (funcall d buf out))))



;;;; Nist test-stuff


(defun make-bit-iterator (len)
  (let ((a (make-array (/ len 8)
		       :initial-element 0
		       :element-type 'unsigned-byte))
	(i 0) (j 0))
    #'(lambda ()
	(fill a 0)
	(setf (aref a i) (ash 1 (- 7 j)))
	(incf j)
	(when (= j 8)      
	  (setf j 0)
	  (incf i))
	a)))
   

(defun ecb-vk-kat (out key-size)  
  (let ((pt (make-array 16 :initial-element 0 :element-type 'unsigned-byte))
	(next-key (make-bit-iterator key-size)))
    (format out "~%~%KEYSIZE=~A~%~%" key-size)
    (format out "PT=~A~%~%" (bin-array->hex-str pt))
    (dotimes (i key-size)
      (let* ((key (bin-array->hex-str (funcall next-key)))
	     (e (make-crypto key :encrypt))
	     (d (make-crypto key :decrypt))
	     (ct (funcall e pt))
	     (pt2 (funcall d ct))) 
	(unless (equalp pt pt2)
	  (error "e(d(pt)) != pt"))
	(format out "I=~A~%" (1+ i))
	(format out "KEY=~A~%" key)
	(format out "CT=~A~%~%" (bin-array->hex-str ct))))
    (format out "==========")))
	 
	
(defun ecb-vt-kat (out key-size)
  (let ((key (make-string (/ key-size 4) :initial-element #\0))
	(next-text (make-bit-iterator +block-bits+)))
    (format out "~%~%KEYSIZE=~A~%~%" key-size)
    (format out "KEY=~A~%~%" key)
    (dotimes (i +block-bits+)
      (let* ((pt (funcall next-text))
	     (e (make-crypto key :encrypt))
	     (d (make-crypto key :decrypt))
	     (ct (funcall e pt))
	     (pt2 (funcall d ct))) 
	(unless (equalp pt pt2)
	  (error "e(d(pt)) != pt"))
	(format out "I=~A~%" (1+ i))
	(format out "PT=~A~%" (bin-array->hex-str pt))
	(format out "CT=~A~%~%" (bin-array->hex-str ct))))
    (format out "==========")))


(defun ecb-mct (&key
		(stream *standard-output*)
		(direction :encrypt)
		(key "00000000000000000000000000000000")
		(in "00000000000000000000000000000000")
		(num-tests 400))
  (let* ((key-size (* 4 (length key)))
	 (cb (hex-str->bin-array in))
	 (cb-old (make-plain-block)))
    (format stream "=========================~%~%")
    (format stream "KEYSIZE=~A~%~%" key-size)
    (dotimes (i num-tests)
      (format stream "I=~A~%" i)
      (format stream "KEY=~A~%" key)
      (format stream "~A=~A~%"
	      (if (eq direction :encrypt) "PT" "CT")
	      (bin-array->hex-str cb))
      (let ((c (make-crypto key direction)))
	(dotimes (j 10000)
	  (replace cb-old cb)
	  (setf cb (funcall c cb cb))))
      (format stream "~A=~A~%~%"
	      (if (eq direction :encrypt) "CT" "PT")
	      (bin-array->hex-str cb))
      (setf key (bin-array->hex-str
		 (map 'vector #'logxor (hex-str->bin-array key)
		      (case key-size
			(128 cb)
			(192 (concatenate 'vector (subseq cb-old 8) cb))
			(256 (concatenate 'vector cb-old cb)))))))))


(defun cbc-mct (&key
		(stream *standard-output*)
		(direction :encrypt)
		(key "00000000000000000000000000000000")
		(in "00000000000000000000000000000000")
		(iv "00000000000000000000000000000000")
		(num-tests 400))
  (let* ((key-size (* 4 (length key)))
	 (in (hex-str->bin-array in))
	 (cb (copy-seq in))
	 (cb-old (make-plain-block)))
    (format stream "=========================~%~%")
    (format stream "KEYSIZE=~A~%~%" key-size)
    (dotimes (i num-tests)
      (format stream "I=~A~%" i)
      (format stream "KEY=~A~%" key)
      (format stream "IV=~A~%" iv)
      (format stream "~A=~A~%"
	      (if (eq direction :encrypt) "PT" "CT")
	      (bin-array->hex-str in))
      (let ((c (make-crypto key direction :mode :CBC :iv iv)))
	(if (eq direction :encrypt)
	    (dotimes (j 10000)
	      (replace cb-old cb)
	      (funcall c in cb)
	      (if (= j 0)
		  (setf in (hex-str->bin-array iv))
		(replace in cb-old)))
	  (dotimes (j 10000)
	    (replace in cb)
	    (funcall c in cb))))
      (format stream "~A=~A~%~%"
	      (if (eq direction :encrypt) "CT" "PT")
	      (bin-array->hex-str cb))      
      (if (eq direction :encrypt)
	  (progn
	    (replace in cb-old)
	    (setf iv (bin-array->hex-str cb)))
	(progn
	  (replace cb-old in)
	  (setf iv (bin-array->hex-str in))
	  (replace in cb)))
      (setf key (bin-array->hex-str
		 (map 'vector #'logxor (hex-str->bin-array key)
		      (case key-size
			(128 cb)
			(192 (concatenate 'vector (subseq cb-old 8) cb))
			(256 (concatenate 'vector cb-old cb)))))))))


(defun make-kats (f filename)
  (with-open-file
   (out filename :direction :output)
   (make-test (format nil "~A (128)" filename) f (list out 128))
   (make-test (format nil "~A (192)" filename) f (list out 192))
   (make-test (format nil "~A (256)" filename) f (list out 256))))


(defun make-vk-kats (&optional (filename "ecb_vk.txt")) 
  (make-kats #'ecb-vk-kat filename))


(defun make-vt-kats (&optional (filename "ecb_vt.txt")) 
  (make-kats #'ecb-vt-kat filename))


(defun make-mcts (f filename)
  (with-open-file
   (out filename :direction :output)
   (make-test (format nil "~A (128)" filename) f
	      (list :stream out :key (make-string 32 :initial-element #\0)))
   (make-test (format nil "~A (192)" filename) f
	      (list :stream out  :key (make-string 48 :initial-element #\0)))
   (make-test  (format nil "~A (256)" filename) f
	      (list :stream out  :key (make-string 64 :initial-element #\0)))
   (format out "===========")))


(defun make-ecb-encrypt-mcts (&optional (filename "ecb_e_m.txt"))
  (make-mcts #'(lambda (&rest args)
		 (apply #'ecb-mct (append '(:direction :encrypt) args)))
	     filename))

					    
(defun make-ecb-decrypt-mcts (&optional (filename "ecb_d_m.txt"))
  (make-mcts #'(lambda (&rest args)
		 (apply #'ecb-mct (append '(:direction :decrypt) args)))
	     filename))


(defun make-cbc-encrypt-mcts (&optional (filename "cbc_e_m.txt"))
  (make-mcts #'(lambda (&rest args)
		 (apply #'cbc-mct (append '(:direction :encrypt) args)))
	     filename))


(defun make-cbc-decrypt-mcts (&optional (filename "cbc_d_m.txt"))
  (make-mcts #'(lambda (&rest args)
		 (apply #'cbc-mct (append '(:direction :decrypt) args)))
	     filename))


(defun make-all-tests ()
  (make-vt-kats)
  (make-vk-kats)
  (make-ecb-encrypt-mcts)
  (make-ecb-decrypt-mcts)
  (make-cbc-encrypt-mcts)
  (make-cbc-decrypt-mcts))


(defun make-test (msg test &optional args)
  (format t "~40A" msg)
  (finish-output)
  (format t "done (~,1Fs)~%" (get-time test args)))


(defun get-time (f &optional args)
  (let ((tm (get-internal-run-time)))
    (apply f args)
    (/ (- (get-internal-run-time) tm)
		  internal-time-units-per-second)))


;; The end

(defun rin->key (rin)
  (let ((str (if (stringp rin) rin (write-to-string rin))))
    (format nil "~a~a~a~a" str str str (subseq str 0 5))))

(defun rin->in (rin)
   (let ((str (if (stringp rin) rin (write-to-string rin)) )
         (in (make-plain-block)))
     (dotimes (i (length in) in)
       (setf (svref in i) (- (char-int (elt str (mod i (length str)))) (char-int #\0))))))

(defun rin->id (rin)
  (let* ((key (rin->key rin))
         (f (make-crypto key  :encrypt))
         (out-vect (funcall f (rin->in rin)))
         (out ""))
    (dotimes (i (length out-vect) out)
      (setf out (concatenate 'string out (write-to-string (elt out-vect i)))))))

(let ((accum nil))
(defun get-accum () accum)
(defun test-rin->id (n)
  (setq accum nil)
  (dotimes ( i n)
    (let ((id (read-from-string (subseq (rin->id (+ 660000000 (random 1000000))) 0 10))))
      (if (find id accum) (break) (push id accum))))
  (length accum)))
