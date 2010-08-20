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

;;; Derived from file : "/usr/include/ppc/types.h"

(fli:define-c-typedef (int8-t (:foreign-name "int8_t")) (:signed :char))
(fli:define-c-typedef (u-int8-t (:foreign-name "u_int8_t"))
                      (:unsigned :char))
(fli:define-c-typedef (int16-t (:foreign-name "int16_t")) :short)
(fli:define-c-typedef (u-int16-t (:foreign-name "u_int16_t"))
                      (:unsigned :short))
(fli:define-c-typedef (int32-t (:foreign-name "int32_t")) :int)
(fli:define-c-typedef (u-int32-t (:foreign-name "u_int32_t"))
                      (:unsigned :int))
(fli:define-c-typedef (int64-t (:foreign-name "int64_t")) :long-long)
(fli:define-c-typedef (u-int64-t (:foreign-name "u_int64_t"))
                      (:unsigned :long-long))
(fli:define-c-typedef (register-t (:foreign-name "register_t")) int32-t)
(fli:define-c-typedef (intptr-t (:foreign-name "intptr_t")) :long)
(fli:define-c-typedef (uintptr-t (:foreign-name "uintptr_t"))
                      (:unsigned :long))

;;; Derived from file : "/usr/include/ppc/ansi.h"

(fli:define-c-typedef (--mbstate-t (:foreign-name "__mbstate_t"))
                      (:union
                       (--mbstate8 (:c-array :char 128))
                       (-mbstatel :long-long)))

;;; Derived from file : "/usr/include/stddef.h"

(fli:define-c-typedef (ptrdiff-t (:foreign-name "ptrdiff_t")) :int)
(fli:define-c-typedef (size-t (:foreign-name "size_t"))
                      (:unsigned :long))
(fli:define-c-typedef (rune-t (:foreign-name "rune_t")) :int)
(fli:define-c-typedef (wchar-t (:foreign-name "wchar_t")) :int)

;;; Derived from file : "/usr/include/gcc/darwin/3.1/stdint.h"

(fli:define-c-typedef (uint8-t (:foreign-name "uint8_t")) u-int8-t)
(fli:define-c-typedef (uint16-t (:foreign-name "uint16_t")) u-int16-t)
(fli:define-c-typedef (uint32-t (:foreign-name "uint32_t")) u-int32-t)
(fli:define-c-typedef (uint64-t (:foreign-name "uint64_t")) u-int64-t)
(fli:define-c-typedef (int-least8-t (:foreign-name "int_least8_t"))
                      int8-t)
(fli:define-c-typedef (int-least16-t (:foreign-name "int_least16_t"))
                      int16-t)
(fli:define-c-typedef (int-least32-t (:foreign-name "int_least32_t"))
                      int32-t)
(fli:define-c-typedef (int-least64-t (:foreign-name "int_least64_t"))
                      int64-t)
(fli:define-c-typedef (uint-least8-t (:foreign-name "uint_least8_t"))
                      uint8-t)
(fli:define-c-typedef (uint-least16-t (:foreign-name "uint_least16_t"))
                      uint16-t)
(fli:define-c-typedef (uint-least32-t (:foreign-name "uint_least32_t"))
                      uint32-t)
(fli:define-c-typedef (uint-least64-t (:foreign-name "uint_least64_t"))
                      uint64-t)
(fli:define-c-typedef (int-fast8-t (:foreign-name "int_fast8_t"))
                      int8-t)
(fli:define-c-typedef (int-fast16-t (:foreign-name "int_fast16_t"))
                      int16-t)
(fli:define-c-typedef (int-fast32-t (:foreign-name "int_fast32_t"))
                      int32-t)
(fli:define-c-typedef (int-fast64-t (:foreign-name "int_fast64_t"))
                      int64-t)
(fli:define-c-typedef (uint-fast8-t (:foreign-name "uint_fast8_t"))
                      uint8-t)
(fli:define-c-typedef (uint-fast16-t (:foreign-name "uint_fast16_t"))
                      uint16-t)
(fli:define-c-typedef (uint-fast32-t (:foreign-name "uint_fast32_t"))
                      uint32-t)
(fli:define-c-typedef (uint-fast64-t (:foreign-name "uint_fast64_t"))
                      uint64-t)
(fli:define-c-typedef (intmax-t (:foreign-name "intmax_t")) :long-long)
(fli:define-c-typedef (uintmax-t (:foreign-name "uintmax_t"))
                      (:unsigned :long-long))

;;; Derived from file : "/System/Library/Frameworks/ApplicationServices.framework/Versions/A/Frameworks/CoreGraphics.framework/Versions/A/Headers/CGError.h"

(fli:define-c-enum (-cgerror (:foreign-name "_CGError"))
                   (kcgerrorsuccess 0)
                   (kcgerrorfirst 1000)
                   (kcgerrorfailure 1000)
                   (kcgerrorillegalargument 1001)
                   (kcgerrorinvalidconnection 1002)
                   (kcgerrorinvalidcontext 1003)
                   (kcgerrorcannotcomplete 1004)
                   (kcgerrornametoolong 1005)
                   (kcgerrornotimplemented 1006)
                   (kcgerrorrangecheck 1007)
                   (kcgerrortypecheck 1008)
                   (kcgerrornocurrentpoint 1009)
                   (kcgerrorinvalidoperation 1010)
                   (kcgerrornoneavailable 1011)
                   (kcgerrorapplicationrequiresnewersystem 1015)
                   (kcgerrorapplicationnotpermittedtoexecute 1016)
                   (kcgerrorlast 1015))
(fli:define-c-typedef (cgerror (:foreign-name "CGError")) int32-t)

;;; Derived from file : "/usr/include/mach/ppc/boolean.h"

(fli:define-c-typedef (boolean-t (:foreign-name "boolean_t")) :int)

;;; Derived from file : "/private/tmp/LWtemp.butane.3283.44.h"

(fli:define-c-struct (cgpoint (:foreign-name "CGPoint"))
                     (x :float)
                     (y :float))

(fli:define-c-typedef (cgpoint (:foreign-name "CGPoint"))
                      (:struct cgpoint))

(fli:define-c-struct (point (:foreign-name "Point"))
  (y :short)
  (x :short))
(fli:define-c-typedef (point (:foreign-name "Point"))
  (:struct point))


(fli:define-c-typedef (cgdisplaycount (:foreign-name "CGDisplayCount"))
                      uint32-t)
(fli:define-c-typedef (cgdisplayerr (:foreign-name "CGDisplayErr"))
                      cgerror)
(fli:define-c-typedef (cgeventerr (:foreign-name "CGEventErr"))
  cgerror)
(fli:define-c-struct (-cgdirectdisplayid
                      (:foreign-name "_CGDirectDisplayID")
                      (:forward-reference t)))
(fli:define-c-typedef (cgdirectdisplayid
                       (:foreign-name "CGDirectDisplayID"))
                      (:pointer (:struct -cgdirectdisplayid)))
(fli:define-c-typedef (time-t (:foreign-name "time_t")) (:unsigned :long)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Definitions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(fli:define-foreign-function (unix-time "time" :source) () :result-type time-t :language :c)

(fli:define-foreign-function (cgdisplayhidecursor "CGDisplayHideCursor" :source)
             ((display cgdirectdisplayid))
           :result-type cgdisplayerr
           :language :c)
         
(fli:define-foreign-function (cgdisplayshowcursor "CGDisplayShowCursor" :source)
    ((display cgdirectdisplayid))
  :result-type cgdisplayerr
  :language :c)

(fli:define-foreign-function (cgdisplaymovecursortopoint "CGDisplayMoveCursorToPoint"
                                                         :source)
                             ((display cgdirectdisplayid)
                              (point cgpoint))
                             :result-type
                             cgdisplayerr
                             :language
                             :c)

(fli:define-foreign-function (cggetactivedisplaylist
                              "CGGetActiveDisplayList"
                              :source)
                             ((maxdisplays cgdisplaycount)
                              (activedspys
                               (:pointer cgdirectdisplayid))
                              (dspycnt (:pointer cgdisplaycount)))
                             :result-type
                             cgdisplayerr
                             :language
                             :c)

(fli:define-c-typedef (cgmousedelta (:foreign-name "CGMouseDelta")) int32-t)

(fli:define-foreign-function (cggetlastmousedelta "CGGetLastMouseDelta" :source)
                             ((deltax (:pointer cgmousedelta))
                              (deltay (:pointer cgmousedelta)))
                             :result-type cgdisplayerr
                             :language :c)

(fli:define-c-typedef (cgbuttoncount (:foreign-name "CGButtonCount")) :int)

(fli:define-foreign-function (getglobalmouse "GetGlobalMouse" :source)
    ((p (:pointer point)))
  :result-type :void
  :language :c)

(fli:define-foreign-function (cgpostmouseevent "CGPostMouseEvent" :source)
    ((mouse-position cgpoint)
     (update-mouse-position (:boolean :int))
     (button-count cgbuttoncount)
     (mouse-button-down (:boolean :int)))
  :result-type :int
  :language :c)

(fli:define-foreign-function (getcurrenteventbuttonstate "GetCurrentEventButtonState" :source)
    ()
  :result-type u-int32-t
  :language :c)

(fli:define-foreign-function (hidemenubar "HideMenuBar" :source)
    ()
  :result-type :void
  :language :c)

(fli:define-foreign-function (showmenubar "ShowMenuBar" :source)
    ()
  :result-type :void
  :language :c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fli:define-c-enum ui-modes
  (kuimodenormal 0)
  (kuimodecontentsuppressed 1)
  (kuimodecontenthidden 2)
  (kuimodeallhidden 3)
  (kuimodeallsuppressed 4))
  
(fli:define-c-enum ui-options
  (kuioptionautoshowmenubar 1) ; 1 << 0
  (kuioptiondisableapplemenu 4) ; 1 << 2
  (kuioptiondisableprocessswitch 8) ; 1 << 3 
  (kuioptiondisableforcequit 16) ; 1 << 4
  (kuioptiondisablesessionterminate 32) ; 1 << 5
  (kuioptiondisablehide 64) ; 1 << 6
  (kuioptiondisablemenubartransparency 512)) ; 1 << 9

(fli:define-c-typedef (systemuimode (:foreign-name "SystemUIMode")) int32-t)
(fli:define-c-typedef (systemuioptions (:foreign-name "SystemUIOptions")) int32-t)
(fli:define-c-typedef (osstatus (:foreign-name "OSStatus")) int32-t)

(fli:define-foreign-function (setsystemuimode "SetSystemUIMode" :source)
    ((in-mode systemuimode)
     (in-options systemuioptions))
  :result-type osstatus
  :language :c)

(defun kiosk-mode (enabled)
  (if enabled
      (setsystemuimode
       (fli:enum-symbol-value 'ui-modes 'kuimodeallhidden)
       (fli:enum-symbol-value 'ui-options 'kuioptionautoshowmenubar))
    (setsystemuimode
     (fli:enum-symbol-value 'ui-modes 'kuimodenormal)
     0)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fli:define-foreign-function (cgcapturealldisplays "CGCaptureAllDisplays" :source)
    ()
  :result-type cgdisplayerr
  :language :c)

(fli:define-foreign-function (cgreleasealldisplays "CGReleaseAllDisplays" :source)
    ()
  :result-type cgdisplayerr
  :language :c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VoiceSpec Structure
(fli:define-c-struct (voice-spec (:foreign-name "VoiceSpec"))
  (creator :int)
  (id :int))

(fli:define-c-typedef (voice-spec (:foreign-name "VoiceSpec"))
  (:struct voice-spec))

;;; Speech Channel Structure
(fli:define-c-struct (speech-channel-record (:foreign-name "SpeechChannelRecord"))
  (data (:c-array :long 1)))

(fli:define-c-typedef (speech-channel-record (:foreign-name "SpeechChannelRecord"))
  (:struct speech-channel-record))

(fli:define-c-typedef (speech-channel (:foreign-name "SpeechChannel"))
  (:struct speech-channel-record))

;;; GetIndVoice Function
;;; Fills VoiceSpec structure with correct information
(fli:define-foreign-function (get-ind-voice  "GetIndVoice" :source)
    ((index :short)
     (voice (:pointer voice-spec)))
  :result-type :int
  :language :c)

;;; NewSpeechChannel Function
;;; Creates a new speech channel with specified voice spec
(fli:define-foreign-function (new-speech-channel "NewSpeechChannel" :source)
    ((v-s (:pointer voice-spec))
     (s-c (:pointer speech-channel)))
  :result-type :int
  :language :c)

;;; SpeakText Function
;;; Speaks string of specified length on speech channel s-c
(fli:define-foreign-function (speak-text "SpeakText" :source)
    ((s-c speech-channel)
     (string (:reference-pass :ef-mb-string))
     (length :int))
  :result-type :int
  :language :c)

(fli:define-foreign-function (sys-err "SysError" :source)
    ((error-code :short))
  :language :c)

;;; Disposes of speech channel s-c
(fli:define-foreign-function (dispose-speech-channel "DisposeSpeechChannel" :source)
    ((s-c speech-channel))
  :result-type :int
  :language :c)

;;; Counts the number of voices
(fli:define-foreign-function (count-voices "CountVoices" :source)
    ((num-voices (:pointer :short)))
  :result-type :int
  :language :c)

;;; Determines if speech is being synthesized elsewhere
;;; 0 if it is not
;;; 1 if it is
(fli:define-foreign-function (speech-busy "SpeechBusy" :source)
    ()
  :result-type :short
  :language :c)

;;;;;;;;;;;;;;;;;;;;;
;; Lisp FF Callers ;;
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

         
(defun hide-cursor ()
  (objc:with-autorelease-pool ()
    (cgdisplayhidecursor (get-active-display))))
         
(defun show-cursor ()
  (objc:with-autorelease-pool ()
    (cgdisplayshowcursor (get-active-display))))




(defun get-button-state ()
  (objc:with-autorelease-pool ()
    (getcurrenteventbuttonstate)))

(defun set-mouse-position (x y)
  (objc:with-autorelease-pool ()
    (fli:with-dynamic-foreign-objects ()
      (let ((point (fli:allocate-foreign-object :type 'cgpoint)))
        (setf (fli:foreign-slot-value point 'x) (float x)
              (fli:foreign-slot-value point 'y) (float y))
        (cgdisplaymovecursortopoint (get-active-display) (fli:dereference point :copy-foreign-object nil))
        ))))

(defun get-mouse-position ()
  (objc:with-autorelease-pool ()
    (let ((pos (objc:invoke "NSEvent" "mouseLocation")))
      (list (aref pos 0) (- *screen-height* (aref pos 1))))))

(defmethod send-mouse-click (&optional &key (duration 0.05))
  (objc:with-autorelease-pool ()
    (fli:with-dynamic-foreign-objects ((point point)
                                       (cg-mouse-point cgpoint))
      (getglobalmouse point)
      (setf (fli:foreign-slot-value cg-mouse-point 'x)
            (float (fli:foreign-slot-value point 'x)))
      (setf (fli:foreign-slot-value cg-mouse-point 'y)
            (float (fli:foreign-slot-value point 'y)))
      (cgpostmouseevent (fli:dereference 
                         cg-mouse-point 
                         :copy-foreign-object nil) t 1 t)
      (sleep duration)
      (cgpostmouseevent (fli:dereference 
                         cg-mouse-point 
                         :copy-foreign-object nil) t 1 nil)
      )))

;; Replaced by set-mouse-position
(defun move-cursor-to-point (x y)
  (objc:with-autorelease-pool ()
    (fli:with-dynamic-foreign-objects ()
      (let ((point (fli:allocate-foreign-object :type 'cgpoint)))
        (setf (fli:foreign-slot-value point 'x) (float x)
              (fli:foreign-slot-value point 'y) (float y))
        (cgdisplaymovecursortopoint (get-active-display) (fli:dereference point :copy-foreign-object nil))
        ))))

;; Replaced by get-mouse-position
(defun mouse-position ()
  (get-mouse-position))

;; Replaced by set-mouse-position
(defun mw-move-cursor (x y)
  (move-cursor-to-point x y))

;; Replaced by single-click-mouse
(defun mw-click-mouse ()
  (objc:with-autorelease-pool ()
    (fli:with-dynamic-foreign-objects ((point point)
                                       (cg-mouse-point cgpoint))
      (getglobalmouse point)
      (setf (fli:foreign-slot-value cg-mouse-point 'x)
            (float (fli:foreign-slot-value point 'x)))
      (setf (fli:foreign-slot-value cg-mouse-point 'y)
            (float (fli:foreign-slot-value point 'y)))
      (cgpostmouseevent (fli:dereference 
                         cg-mouse-point 
                         :copy-foreign-object nil) t 1 t)
      (sleep 0.05)
      (cgpostmouseevent (fli:dereference 
                         cg-mouse-point 
                         :copy-foreign-object nil) t 1 nil)
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-active-display ()
  (objc:with-autorelease-pool ()
    (fli:with-dynamic-foreign-objects ((activedspys cgdirectdisplayid)
                                       (dspycnt cgdisplaycount))
      (cggetactivedisplaylist 1 activedspys dspycnt)
      (fli:dereference activedspys))))

(defun capture-all-displays ()
  (objc:with-autorelease-pool ()
    (fli:with-dynamic-foreign-objects ()
      (cgcapturealldisplays))))

(defun release-all-displays ()
  (objc:with-autorelease-pool ()
    (fli:with-dynamic-foreign-objects ()
      (cgreleasealldisplays))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu bars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hide-menu-bar ()
  (objc:with-autorelease-pool ()
    (fli:with-dynamic-foreign-objects ()
      (hidemenubar))))

(defun show-menu-bar ()
  (objc:with-autorelease-pool ()
    (fli:with-dynamic-foreign-objects ()
      (showmenubar))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speech stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Checks availible voices to see if selected voice ID exists
(defun check-voices (voice-num)
  (block nil
    (let ((voices (fli:allocate-foreign-object :type :short)))
      (count-voices voices)
      (if (or (> voice-num (fli:dereference voices))
              (< voice-num 1))
          (progn
            (format t "ERROR: MUST SELECT A VOICE BETWEEN 1 AND ~A" 
                    (fli:dereference voices))
            (return nil)))
      t)))

;;; Checks if other speech is being synthesized
;;; if so, waits until it finishes
(defun other-speech-being-synthesized ()
  (do
      ((busy (speech-busy)))
      ((= busy 0))
    (sleep 0.5)
    (setf busy (speech-busy))))

;;; Allocates memory to a voice spec and returns the pointer to it
(defun return-voice-spec ()
  (fli:allocate-foreign-object :type 'voice-spec))

;;; Deallocates memory associated with voice spec vs
(defun free-voice-spec (vs)
  (fli:free-foreign-object vs))

;;; Allocates memory to a speech channel and returns the pointer to it
(defun return-speech-channel ()
  (fli:allocate-foreign-object :type 'speech-channel))

;;; Deallocates memory associated with speech channel sc
(defun free-speech-channel (sc)
  (fli:free-foreign-object sc))

;;; Deallocates memory for both the speech channel and the voice-spec
(defun free-all-memory (sc vs)
  (free-speech-channel sc)
  (free-voice-spec vs))

;;; Disposes of speech channel sc when no longer wanted
(defun kill-speech-channel (sc)
  (dispose-speech-channel (fli:dereference sc :copy-foreign-object nil)))

;;; Creates a new speech channel based on voice-id
;;; structs voice-spc and speech-ch are filled after it executes
;;; Returns nil if invalid voice is selected
(defun create-new-speech-channel (voice-id voice-spc speech-ch)
  (objc:with-autorelease-pool ()
    (if (check-voices voice-id)
        (progn
          (get-ind-voice voice-id voice-spc)
          (new-speech-channel voice-spc speech-ch)))))

;;; Speaks str using on speech channel speech-ch
;;; Voice ID and VoiceSpec must have been initialized already
(defun speak-phrase (str speech-ch)
  (objc:with-autorelease-pool ()
    ;; Waits for any other speech sythesis to finish
    (other-speech-being-synthesized)

    ;; Because :copy-foreign-object is nil, returns object directly
    ;; just as SpeakText function needs
    (speak-text (fli:dereference speech-ch :copy-foreign-object nil) 
                str 
                (length str))))

(defun speak-string (str &key (voice 3))
  (objc:with-autorelease-pool ()
    (block nil
      (let ((sc (return-speech-channel))
            (vs (return-voice-spec)))
        
        ;; Checks to see if voice selected exists
        (if (equal 0 (create-new-speech-channel voice vs sc))
            (speak-phrase str sc)
          (return nil))
        
        (if (equal (control-mode *mw*) :ACT-R)
            (new-digit-sound str))
        
        ;; Waits for current speech sythesis to finish
        (other-speech-being-synthesized)
        
        ;; Disposes of speech channel and frees memory
        (kill-speech-channel sc)
        (free-voice-spec vs)
        (free-speech-channel sc))
      t)))

;; Replaced by speak-string
;;  
;;; All-in-one function -> keeps all variables local
;;; Speak the given string str with the voice of ID voice-num
;;; Will return nil if an invalid voice is selected
(defun cw-speak (str &optional &key (voice 3) (model-string nil))
  (objc:with-autorelease-pool ()
    (block nil
      (let ((sc (return-speech-channel))
            (vs (return-voice-spec)))
        
        ;; Checks to see if voice selected exists
        (if (equal 0 (create-new-speech-channel voice vs sc))
            (speak-phrase str sc)
          (return nil))
        
        (if (equal (control-mode *cw*) :ACT-R)
            (if model-string (new-word-sound model-string)
              (new-word-sound str)))
        
        ;; Waits for current speech sythesis to finish
        (other-speech-being-synthesized)
        
        ;; Disposes of speech channel and frees memory
        (kill-speech-channel sc)
        (free-voice-spec vs)
        (free-speech-channel sc))
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Play a sound file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun play-sound-file (str)
  (objc:with-autorelease-pool ()
    (let ((url (objc:invoke "NSURL"  "fileURLWithPath:" str)))
      ;; Trying to execute [[[NSSound alloc] initWithContentsOfFile:url FALSE] play]
      ;; to play file at url (is only a fragment of what actually needs to be called, assuming LispWorks
      ;; takes care of it for us)
      (objc:invoke (objc:invoke (objc:invoke "NSSound" "alloc")
                                "initWithContentsOfURL:byReference:"
                                url nil) "play"))))