(defpackage "CRYPT"
  (:use "COMMON-LISP")
  (:export "RIN->ID"))

(in-package :crypt)

(defun get-cipher (key) 
  (ironclad:make-cipher :aes 
                        :mode :cbc
                        :initialization-vector (ironclad:ascii-string-to-byte-array "0000000000000000")
                        :key (ironclad:ascii-string-to-byte-array key)))

(defun encrypt (plaintext key)
  (let ((cipher (get-cipher key))
        (msg (ironclad:ascii-string-to-byte-array plaintext)))
    (ironclad:encrypt-in-place cipher msg)
    (ironclad:byte-array-to-hex-string msg)))

(defun rin->key (rin)
  (let ((str (if (stringp rin) rin (write-to-string rin))))
    (format nil "~a~a" str (subseq str 0 7))))

(defun rin->id (rin)
  (let ((key (rin->key rin)))
    (encrypt key key)))