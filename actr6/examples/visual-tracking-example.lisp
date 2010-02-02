;;; Examples of visual tracking using the old style with an object
;;; based visicon (the currently provided devices) and with a custom
;;; device that uses a chunk based visicon (without having explicit
;;; objects set for the items).

(defun object-tracking () ;; old style with a screen object
  
  (reset)
  (let* ((window (open-exp-window "Moving X" :visible nil))
         (letter (add-text-to-exp-window :text "x" :x 10 :y 150)))
    
    (if (not (subtypep (type-of window) 'virtual-window))
        (print-warning "This example only works correctly for virtual and visible-virtual windows.")
      
      (progn
        (install-device window)
        (proc-display)
        (schedule-periodic-event .5 #'(lambda () 
                                        (remove-items-from-exp-window letter)
                                        
                                        ;; Virtual dialog item specific coordinate moving
                                        ;; code.  Code for real windows is different for each
                                        ;; Lisp...
                                        
                                        (setf (x-pos letter) (+ 10 (x-pos letter)))
                                        
                                        (add-items-to-exp-window letter)
                                        
                                        (update-tracking)
                                        ; could also call 
                                        ;(proc-display)
                                        )
                                 :details "moving object"
                                 :initial-delay 1.0)
        
        (run 3)))))



;;; new style with a custom device and using the visual-location chunks directly
;;
;;; First define the normal methods for a simple list based device.

(defmethod device-move-cursor-to ((device list) loc)
  ;; ignore model's mouse move requests
  nil)

(defmethod get-mouse-coordinates ((device list))
  ;; always return the same location for the mouse
  (vector 0 0))

(defmethod device-handle-click ((device list))
  ;; ignore a mouse click
 nil)

(defmethod device-handle-keypress ((device list) key)
  ;; ignore key presses
  nil)

(defmethod device-speak-string ((device list) string)
  ;; ignore model's speech output
  nil)


(defmethod cursor-to-vis-loc ((device list))
  nil)


(defmethod build-vis-locs-for ((device list) vis-mod)
  ;; just return the cars from all the sublists
  (mapcar 'car device))


(defmethod vis-loc-to-obj ((device list) vis-loc)
  ;; here we're just returning the pregenerated object from the list
  (cdr (assoc vis-loc device)))



(defun chunk-based-tracking ()
  
  ;; Start by resetting the model.
  
  (reset)
  
  ;; First create the visual-location chunk
  
  (let* ((visual-location-chunks (define-chunks 
                                     (isa visual-location screen-x 15 screen-y 20 kind text value text height 10 width 40 color blue)))
         
         ;;; and the visual-object chunk
         
         (visual-object-chunks (define-chunks
                                   (isa visual-object value "x" height 10 width 40 color blue)))
         
         (the-device (pairlis visual-location-chunks visual-object-chunks)))
    
    ;; make that the current device for the model
    
    (install-device the-device)
    
    ;; process the display 
    
    (proc-display)
    
    ;; schedule an event to move the item
    
    (schedule-periodic-event .5 #'(lambda () 
                                    (let* ((c (car visual-location-chunks))
                                           (x (chunk-slot-value-fct c 'screen-x)))
                                      
                                      ;; just increment the screen-x slot of the
                                      ;; visual-location chunk that's passed to the 
                                      ;; vision module for the visicon.
                                      
                                      (set-chunk-slot-value-fct c 'screen-x (+ 10 x))
                                        
                                      (update-tracking)
                                      ; could also call (proc-display) if there were more
                                      ; things to be updated on the screen
                                        ))
                                 :details "moving object"
                                 :initial-delay 1.0)
    
    ;; run the model
    (run 3)))





(clear-all)

(define-model simple-tracking

(sgp :v t :needs-mouse nil :show-focus t :trace-detail high)
  
(P found-letter
   
   =visual-location>
      ISA         visual-location
   
   ?visual>
      state        free
   
==>
   +visual>
      ISA         move-attention
      screen-pos  =visual-location
)

(P track-letter
   =visual>
      ISA         visual-object
      value       =letter
   ?visual>
      state       free
   ==>
   +visual>
      isa         start-tracking
)

(p report
   =visual-location>
      isa         visual-location
      screen-x    =x
      screen-y    =y
   =visual>
      isa         visual-object
      value       =letter
   ==>
   !output! (The =letter is at =x =y)
   )
)
