(defstruct (buffer-info (:conc-name bi-) (:type list))
  "stores read in buffer information from ACT-r trace"
  (beg-sys-tm nil)        ;beginning system time
  (end-sys-tm nil)        ;ending system time
  (beg-act-tm nil)        ;beginning act-r time
  (end-act-tm nil)        ;ending act-r time
  (module nil)            ;buffer information (vision, DM, motor, production, etc.)
  (action nil)            ;action performed by buffer (RHS of production)
  (param nil))

(let ((my-events nil)
      (current-motor-action nil)
      (current-goal-type nil)
      (start-time nil)
      (var-list
  '(
    PRODUCTION_SPEC
    PRODUCTION_GEN
    DEC-MEM
    VISION_SPEC 
    VISION_GEN
    MOTOR_SPEC 
    MOTOR_GEN  
    AURAL_SPEC 
    AURAL_GEN
    ))
      (event-list 
  '(
    click-mouse
    key-press
    move-cursor
    finish-movement
    find-location
    find-loc-failure 
    move-attention
    encoding-complete
    start-retrieval
    retrieved-chunk
    retrieval-failure
    attend-sound
    production-selected 
    production-fired 
    set-buffer-chunk
    create-new-buffer-chunk
;    preparation-complete
;    initiation-complete
;    move-cursor-absolute
     
)))

(defun get-var-list () var-list)
(defun set-var-list (v) (setq var-list v))


(defun get-event-list () event-list)
(defun set-event-list (v) (setq event-list v))

#+:lispworks 
(progn
(capi:define-interface cmp-screen ()
  ()
  (:panes
   (collect-lp
    capi:double-list-panel
    :title "Collect"
    :interaction :multiple-selection
    :items (get-event-list)
    :print-function 'string-downcase
    :visible-min-width 500
    :callback-type :data-interface
    :selection-callback 'collect-panel-selection-callback
    :retract-callback   'collect-panel-retract-callback)
   (var-lp
    capi:list-panel
    :title "Variables"
    :interaction :multiple-selection
    :items (get-var-list)
    :print-function 'string-downcase
    :visible-min-width 250
    :callback-type :data-interface
    :selection-callback 'var-panel-selection-callback
    :retract-callback   'var-panel-retract-callback)
   )
  (:layouts
   (main-layout
    capi:row-layout
    '(collect-lp var-lp)))
  (:default-initargs
   :title "Cognitive Metric Profiling"
   :best-width 800
   :best-height 800))


;;----------------------------------------------------------------------------
;; callbacks
;;----------------------------------------------------------------------------

(defun collect-panel-selection-callback (data interface)
  (push data event-list) )

(defun collect-panel-retract-callback (data interface)
  (setq event-list (remove data event-list)))

(defun var-panel-selection-callback (data interface)
  (push data var-list) )

(defun var-panel-retract-callback (data interface)
  (setq var-list (remove data var-list)))


(defun make-cmp-screen ()
  (capi:display (make-instance 'cmp-screen)))
) ;;end of lispworks


(defun get-cmp-time ()
  (aif start-time (- (get-internal-real-time) it) (progn (setq start-time (get-internal-real-time)) 0))) 

(defvar *all-actr-events* nil)

(defun my-event-hook (ev)
 (push ev *all-actr-events*)
 ;(if (eql 'encoding-complete (evt-action ev)) (format *actr-output* "~%encoding complete event"))
 (if (member (evt-action ev) event-list)  
  (case (evt-action ev)
    ((click-mouse move-cursor key-press)
     (push (make-buffer-info :beg-sys-tm (get-cmp-time) :beg-act-tm (evt-time ev)
                             :module (evt-module ev) :action (evt-action ev))
           my-events)
     (setq current-motor-action (first my-events)))
    ((find-location move-attention  start-retrieval attend-sound find-loc-failure )
     (push (make-buffer-info :beg-sys-tm (get-cmp-time) :beg-act-tm (evt-time ev)
                             :module (evt-module ev) :action (evt-action ev))
           my-events))
    ((production-selected production-fired)
     (push (make-buffer-info :beg-sys-tm (get-cmp-time) :beg-act-tm (evt-time ev)
                             :module (evt-module ev) :action (evt-action ev) :param (production-name (first (evt-params ev)))  )
           my-events))
    (create-new-buffer-chunk
     (let ((goal-typ (second (second  (evt-params ev)))))
       (push (make-buffer-info :beg-sys-tm (get-cmp-time) :beg-act-tm (evt-time ev)
                             :module (evt-module ev) :action 'switch-goal :param goal-typ)
             my-events)
       (setq current-goal-type goal-typ)))
   
    ((retrieved-chunk retrieval-failure)
       (fill-in-event (find 'start-retrieval my-events :key #'bi-action) ev (eql (evt-action ev) 'retrieved-chunk)) )
    (set-buffer-chunk
     (case (evt-module ev)
       (:vision
        (case (first (evt-params ev))
          (visual-location
           (fill-in-event (find 'find-location  my-events :key #'bi-action) ev t))
          (visual
           (fill-in-event (find 'move-attention  my-events :key #'bi-action) ev t))))
       (:audio
        (case (first (evt-params ev))
          (aural-location
           (push (make-buffer-info :beg-sys-tm (get-cmp-time) :beg-act-tm (evt-time ev)
                             :module (evt-module ev) :action 'find-sound)
           my-events))
          (aural
           (fill-in-event (find 'attend-sound  my-events :key #'bi-action) ev t))))
       (goal
        (when (null current-goal-type)
          (let ((type-name (act-r-chunk-type-name (act-r-chunk-chunk-type (get-chunk (second (evt-params ev)))))))
            (push (make-buffer-info :beg-sys-tm (get-cmp-time) :beg-act-tm (evt-time ev)
                             :module (evt-module ev) :action 'switch-goal :param type-name)
             my-events)
            (setq current-goal-type  nil))))))
    (finish-movement
     (fill-in-event current-motor-action ev t))  
#|
    (otherwise
     (push (make-buffer-info :beg-sys-tm (get-cmp-time) :beg-act-tm (evt-time ev)
                             :module (evt-module ev) :action (evt-action ev) :param (evt-params ev))
             my-events))
|# 
)))

(defun enable-cmp (&key events)
  (if events (setq event-list events))
  (add-pre-event-hook #'my-event-hook))

(defun fill-in-event (obj ev param-val)
  (when obj
         (setf (bi-end-sys-tm obj) (get-cmp-time))
         (setf (bi-end-act-tm obj) (evt-time ev))
         (setf (bi-param obj) param-val)))

(defun clear-my-events ()
  (setq my-events nil))

(defun get-my-events ()
  my-events)

(defun set-my-events (v)
  (setq my-events v))
#|
(defmethod (setf loc-failure) :after (val (m attn-module))
  (when val
    (push (make-act-r-event :time (mp-time) :action 'loc-failure :model (current-model)  :module (my-name m )) my-events)))
|#

) ;;end of events



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;--------------------------------------------------------- File Processing --------------

(defvar m-columns ())     ;for Dan's MacSHAPA functions
(defvar m-cells ())       ;for Dan's MacSHAPA functions
(defvar *total-data* nil) ;stores all simborg buffer-info structures

       




#+:lispworks 
(defun write-events ()
  (let* ((events (reverse (get-my-events)))
         (pn (capi:prompt-for-directory "Select folder for file"))
         (fn (capi:prompt-for-string "Enter File Name")))
    (write-event-file (merge-pathnames fn pn) events)))
#+:digitool
(defun write-events (&optional filename )
  (let ((events (reverse (get-my-events))))
    (if (null filename)
      (let* ((pn (choose-directory-dialog :prompt "Select folder for file"))
             (fn (choose-new-file-dialog :prompt "Enter File Name")))
        (setq filename (merge-pathnames fn pn))))       
    (write-event-file filename events)))
#|
(defun write-event-file (fn events)
  (with-open-file (fs fn :direction :output)
      (dolist (ev events)
         (write ev  :stream fs) 
         (write-char #\newline fs))))
|#

(defun write-event-file (fn events)
  (with-open-file (fs fn :direction :output)
    (dolist (ev events)
      (dolist (e ev)
        (write e  :stream fs) (write-char #\tab fs)) 
      (write-char #\newline fs))))

(defun write-events-to-file (fn events)
  (with-open-file (fs fn :direction :output)
    (dolist (ev events)
      
        (write ev  :stream fs) (write-char #\newline fs)) 
      ))



(defstruct (prod-count (:type list)) name count p c pg-c utility)

(defun count-prods (&key (file t))
  (let* ((events (reverse (get-my-events)))
         (all-prods (mapcan (lambda (x) (if (eql (bi-action x) 'production-fired) (list (bi-param x)))) events))
         (unique-prods (remove-duplicates all-prods))
         (prods (sort (mapcar (lambda(x) (let ((prod (get-production-by-name x)))
                                           (make-prod-count :name x :count (count x all-prods)
                                                            :p (production-p prod) :c (production-c prod)
                                                            :pg-c (production-pg-c prod)
                                                            :utility (production-utility prod))))
                              unique-prods) #'string< :key #'first)))
    (cond (file
           (if (null (stringp file))
             (let ((pn (choose-directory-dialog :prompt "Select folder for file"))
                   (fn (choose-new-file-dialog :prompt "Enter File Name")))
               (write-event-file (merge-pathnames fn pn) prods))
             (write-event-file file prods)))
           (t
            prods))))
            

(defun filter-events (start-time end-time &optional (fn #'bi-beg-act-tm))
   (set-my-events  (remove-if-not (lambda(x) (<= start-time x  end-time)) (get-my-events) :key fn)))


(defun printlist (var)
  "takes any list as input and prints item location w/in list (1,2,3,etc.) followed by item"
  (let ((count 1))
    (dolist (element var)
      (print count)
      (print element)
      (incf count))))

(defun conv-to-list (str)
  "converts a string to a list of elements"
  (cond (str (read-from-string (concatenate 'string "(" (string-trim '(#\Space #\. #\?) str) ")")))))


(defun cmp->macshapa (&key (events (reverse (get-my-events))) file )
  "Processes stored buffer-info structures into MacSAHAP file format"
  (m-init (list "PRODUCTION_SPEC" "PRODUCTION_GEN" "DEC-MEM"  "VISION_SPEC" "VISION_GEN" "MOTOR_SPEC" "MOTOR_GEN"  "AURAL_SPEC" "AURAL_GEN"))
  (dolist (data events)
    (case (bi-module data)
     (PROCEDURAL
      (when (eql (bi-action data) 'production-fired)
          (let ((tm (bi-beg-sys-tm data)))
            (m-create-cell "PRODUCTION_SPEC" tm  (+ tm 50) (format nil "production firing ~s"  (bi-param data)))
            (m-create-cell "PRODUCTION_GEN" tm  (+ tm 50) (format nil "production firing" )))))
     (:VISION
      (let ((end-time (aif (bi-end-sys-tm data) it (+ (bi-beg-sys-tm data) 50))))
        (m-create-cell "VISION_SPEC" (bi-beg-sys-tm data) end-time (format nil "~s" (bi-action data)))
        (m-create-cell "VISION_GEN" (bi-beg-sys-tm data) end-time  (format nil "vision"))))
     (:MOTOR
       (let ((end-time (aif (bi-end-sys-tm data) it (+ (bi-beg-sys-tm data) 50))))
         (m-create-cell "MOTOR_SPEC" (bi-beg-sys-tm data) end-time (format nil "~s" (bi-action data)))
         (m-create-cell "MOTOR_GEN" (bi-beg-sys-tm data) end-time (format nil "motor"))))
     (DECLARATIVE
      (let ((end-time (aif (bi-end-sys-tm data) it (+ (bi-beg-sys-tm data) 50))))
        (m-create-cell "DEC-MEM" (bi-beg-sys-tm data) end-time  (format nil "~s" (bi-action data)))))
     (:AUDIO
      (let ((end-time (aif (bi-end-sys-tm data) it (+ (bi-beg-sys-tm data) 50))))
        (m-create-cell "AURAL_SPEC" (bi-beg-sys-tm data) end-time (format nil "~s" (bi-action data)))
        (m-create-cell "AURAL_GEN" (bi-beg-sys-tm data) end-time (format nil "aural"))))))
#+:digitool (m-save-file (aif file it (choose-new-file-dialog :prompt "Save Macshapa File As...")))
#+:lispworks  (m-save-file (aif file it (capi:prompt-for-file "Save Macshapa File As..."
                                     :operation :save
                                     :if-exists :ok
                                     :pathname (current-pathname "macshapa.txt")
                                     :filter "*.txt")))
)

;---------------------------------------------------------------- MacSHAPA Helpers (Dan) ----------------

(defun m-init (column-list)
  "Initializes macshapa columns."
  (setq m-columns column-list)
  (setq m-cells (make-list (length m-columns) :initial-element '())))

(defun m-create-cell (column-name timestart-ms timeend-ms cell-value)
  "Creates a macshapa cell in one of the columns."
  (let* ((timestart (round (* (/ timestart-ms 1000) 60)))
           (timeend (round (* (/ timeend-ms 1000) 60))))
    ;(PRINT COLUMN-NAME)
    (push (format nil "~s	~s	 ~s ~a " timestart timeend (length cell-value) cell-value)
          (nth (position column-name m-columns :test #'equal) m-cells))))

(defun m-save-file (file-name)
  "Saves all data to a file."
  (with-open-file (fileout file-name :direction :output :if-exists :supersede)
    (write-line "***MacShapa Version***
0 0 a
***Predicates***" fileout)
    (write-line (write-to-string (length m-columns)) fileout)
    (dolist (column-name m-columns)
      (write-line (format nil " 0 1  ~s ~a(<ord>,<onset>,<offset>,<val>) "
                          (+ (length column-name) 30) column-name) fileout))
    (write-line "***Variables***" fileout)
    (write-line (write-to-string (length m-columns)) fileout)
    (dolist (column-name m-columns)
      (write (length (nth (position column-name m-columns :test #'equal) m-cells)) :stream fileout)
      (write-line "	nomID	0	0	200" fileout)
      (write-line (format nil " ~s ~a(<ord>,<onset>,<offset>,<val>) "
                          (+ (length column-name) 30) column-name) fileout)
      (dolist (line (nth (position column-name m-columns :test #'equal) m-cells))
        (write-line line fileout))
      (write-line "0" fileout))
    (write-line "***SpreadPane***" fileout)
    (write-line (format nil " ~s" (length m-columns)) fileout)
    (dolist (column-name m-columns)
      (write-line (format nil " ~s ~a " (length column-name) column-name) fileout))
    (write-line "***Groups***" fileout)
    (write-line (format nil "---VarMap---~s" (length m-columns)) fileout)
    (dolist (column-name m-columns)
      (write-line column-name fileout))
    (write-line "%
***GrammarFormats***
$" fileout))
  t)






