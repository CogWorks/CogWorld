;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename     : foreign.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Author       : Chris R. Sims
;; Copyright    : (C) 2005 Chris R. Sims
;; Address      : Cognitive Science Department
;;              : Rennselaer Polytechnic Institute
;;              : Troy, NY 12180
;;              : simsc@rpi.edu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Version      : 2.0.0
;; Description  : Contains all platform-specific code & foreign function calls
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; History
;;
;; [2005.03.15] : File created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fli:define-c-typedef bool (:boolean :int))
(fli:define-c-typedef long :long)

(fli:define-c-struct tagpoint
                    (x long)
                    (y long))
(fli:define-c-typedef point (:struct tagpoint))
(fli:define-c-typedef lppoint (:pointer point))

(fli:define-foreign-function (get-cursor-position "GetCursorPos")
    ((lp-point lppoint))
  :result-type bool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-button-state ()
  )

(defun set-mouse-position (x y)
  )

(defun get-mouse-position ()
  (let ((location (fli:allocate-foreign-object :type 'point)))
    (get-cursor-position location)
    (list (fli:foreign-slot-value location 'x)
          (fli:foreign-slot-value location 'y)))
  )

(defmethod send-mouse-click (&optional &key (duration 0.05))
  )

;; Replaced by set-mouse-position
(defun move-cursor-to-point (x y)
  )

;; Replaced by get-mouse-position
(defun mouse-position ()
  (get-mouse-position)
  )

;; Replaced by set-mouse-position
(defun mw-move-cursor (x y)
  )

;; Replaced by single-click-mouse
(defun mw-click-mouse ()
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-active-display ()
  )

(defun capture-all-displays ()
  )

(defun release-all-displays ()
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu bars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hide-menu-bar ()
  )

(defun show-menu-bar ()
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speech stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Checks availible voices to see if selected voice ID exists
(defun check-voices (voice-num)
  )

;;; Checks if other speech is being synthesized
;;; if so, waits until it finishes
(defun other-speech-being-synthesized ()
  )

;;; Allocates memory to a voice spec and returns the pointer to it
(defun return-voice-spec ()
  )

;;; Deallocates memory associated with voice spec vs
(defun free-voice-spec (vs)
  )

;;; Allocates memory to a speech channel and returns the pointer to it
(defun return-speech-channel ()
  )

;;; Deallocates memory associated with speech channel sc
(defun free-speech-channel (sc)
  )

;;; Deallocates memory for both the speech channel and the voice-spec
(defun free-all-memory (sc vs)
  )

;;; Disposes of speech channel sc when no longer wanted
(defun kill-speech-channel (sc)
  )

;;; Creates a new speech channel based on voice-id
;;; structs voice-spc and speech-ch are filled after it executes
;;; Returns nil if invalid voice is selected
(defun create-new-speech-channel (voice-id voice-spc speech-ch)
  )

;;; Speaks str using on speech channel speech-ch
;;; Voice ID and VoiceSpec must have been initialized already
(defun speak-phrase (str speech-ch)
  )

(defun speak-string (str &key (voice 19))
  )

;; Replaced by speak-string
;;  
;;; All-in-one function -> keeps all variables local
;;; Speak the given string str with the voice of ID voice-num
;;; Will return nil if an invalid voice is selected
(defun mw-speak (str &optional &key (voice 19) (model-string nil))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Play a sound file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun play-sound-file (str)
  )