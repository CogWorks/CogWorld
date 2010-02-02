;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Dan Bothell
;;; Copyright   : (c) 2005 Dan Bothell
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Department of Psychology 
;;;             : Carnegie Mellon University
;;;             : Pittsburgh, PA 15213-3890
;;;             : db30@andrew.cmu.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : p-star-cmd.lisp
;;; Version     : 1.1
;;; 
;;; Description : Functions that work with the procedural module to allow
;;;             : definition of productions with bound-variable slot names.
;;; 
;;; Bugs        : 
;;;
;;; To do       : [ ] Modifiy the ordering of conditions so that one can
;;;             :     use an explicit bind for a variablized slot as long as
;;;             :     there aren't circularities.  Not quite as easy as I
;;;             :     inititial thought because you need to differentiate
;;;             :     explicit binds based on whether they should occur before 
;;;             :     or after the variablized slot tests.
;;; 
;;; ----- History -----
;;; 2005.02.05 Dan
;;;             : * Creation - no 80 column limit because it really makes it 
;;;                 hard for me to work with this!
;;; 2005.02.28 Dan
;;;             : * Made the replace-variables-for-eval fix in p* as well.
;;; 2005.04.26 Dan
;;;             : * Restored the setting of lhs and rhs of the production to
;;;             :   the parsed code for use by compilation.
;;; 2005.05.03 Dan
;;;             : * Added the production-?hs-buffers setting for compilation.
;;; 2005.05.04 Dan
;;;             : * Fixed the issue with direct requests not clearing the
;;;             :   buffer.
;;; 2005.05.11 Dan
;;;             : * Changed the output parameter for buffer clearing so that
;;;             :   it prints in the medium traces as well.
;;; 2005.06.19 Dan
;;;             : * Seems to be a bug wrt condition ordering if there are
;;;             :   "cross buffer" conditions.
;;; 2005.06.21 Dan
;;;             : * Fixed the issue noted above.  What happened is that a 
;;;             :   variablized slot with a variablized value was being allowed
;;;             :   to rebind the variable even if it had a different value.
;;;             :   Doesn't do that now.
;;; 2005.09.01 Dan
;;;             : * Making the changes necessary to allow a RHS = in a p* to
;;;             :   extend the chunk-type definition.
;;;             : * Had to add a flag to the procedural module (can't use a
;;;             :   global and p*'s don't have their own module) called
;;;             :   check-p*-mods to let compilation generate correctly in
;;;             :   all cases.
;;;             ; * Adding a test of the above to valid-variable-chunk-mod-spec
;;;             :   to allow production compilation to create instantiated
;;;             :   p*'s for slots that don't exist yet.
;;;             : * Adding the extend-buffer-chunk function and putting it into
;;;             :   the RHS = action function before the mod.
;;; 2005.09.09 Dan
;;;             : * Changed references of chunk-to-chunk-spec to chunk-name-to-
;;;             :   chunk-spec and made sure to call it appropriately.
;;; 2005.09.14 Dan
;;;             : * Change to the priority of RHS actions. = is now set to a
;;;             :   higher priority than +.  See log in procedural.lisp for
;;;             :   the detailed rational.
;;; 2005.12.05 Dan
;;;             : * Fixed an issue with ordering when a variablized slot needs
;;;             :   to create a binding, but the buffer that binds the slot
;;;             :   name occurs in a buffer test after the buffer with the
;;;             :   variablized slot.  Added replace-slot-variables to handle
;;;             :   that and modified p*-fct appropriately.
;;;             :   - Should probably just fix replace-variables, but don't
;;;             :   want to deal with the possible issues that could cause.
;;; 2005.12.06 Dan
;;;             : * Previous fix is not quite sufficient because there's
;;;             :   still an issue with the binding of that variablized slot's
;;;             :   value.  Changing the matching process slightly so that
;;;             :   it's a more explicit 2 steps of binding and now it rejects
;;;             :   p*'s that violate the two step rule.
;;; 2005.12.07 Dan
;;;             : * Following the changes to the p command, p*'s are now
;;;             :   more restrictive with respect to !bind! and most problem
;;;             :   conditions (circular references, unbound vars, etc) should
;;;             :   now be flagged.  The biggest change is that a !bind! cannot
;;;             :   be used to set a variable for a slot name anymore - all
;;;             :   explicit binds take place after all the buffer bindings.
;;; 2005.12.12 Dan
;;;             : * The last update caused some problems for production
;;;             :   compilation because it modified the order of the lhs data
;;;             :   in the p* relative to p.  That's been corrected now.
;;; 2006.01.18 Dan
;;;             : * Eliminated some warnings that occured during conflict 
;;;             :   resolution because a variablized slot didn't name a valid
;;;             :   slot in the chunk-type at that point.  Now, it does a
;;;             :   round of slot validation first.
;;; 2006.01.31 Dan
;;;             : * Fixed a bug that was introduced with the elimination of
;;;             :   the warnings that really broke the p* mechanism...
;;; 2006.03.21 Dan
;;;             : * Fixed a bug that slipped through when I update the
;;;             :   utility mechanisms.
;;; 2006.10.11 Dan
;;;             : * Changed the priority of the buffer-overwrite action to be
;;;             :   90 instead of 100 (the priority of the buffer mod action)
;;;             :   because the modifications must occur first, but if they
;;;             :   have the same priority then they could be reversed which
;;;             :   can lead to run-time issues and isn't right with respect
;;;             :   to how productions should work.
;;; 2006.11.09 Dan 
;;;             : * Added the define-p* and define-p*-fct commands for consistency.
;;;             : * Added the setting of the dynamic slot in the production
;;;             :   structure of a p*.
;;;             : * Modified the call to print-production-output in the parsing
;;;             :   of an !output! command so that the "old style" format string
;;;             :   usage doesn't get triggered implicitly.
;;;             : * Fixed a bug in production parsing that would lead to run time
;;;             :   errors if a buffer overwrite action didn't specify a chunk
;;;             :   or at least a variable (the variable could still bind to a
;;;             :   non-chunk at run time).
;;; 2006.11.10 Dan
;;;             : * Additional update to the parsing like the last one - now
;;;             :   direct requests must be a variable or chunk.
;;;             : * Fixed a cut and paste error related to the last change.
;;; 2006.11.20 Dan
;;;             : * Removed the special case implicit action for visual requests in
;;;             :   the production definition and replaced it with the new 
;;;             :   (generic) module-warning mechanims.
;;; 2007.06.18 Dan
;;;             : * Changed calls to slot-specs-to-chunk-spec-list to add the -fct
;;;             :   since it's now an "official" command.
;;; 2008.03.24 Dan
;;;             : * Start of the work to add the !mv-bind! to productions.
;;;             : * Added the failure reason for !bind! so whynot shows that it
;;;             :   fails for a binding to nil.
;;;             : * Sort the conditions of the productions now so that explicit
;;;             :   binds always precede buffer bindings (thought I did that
;;;             :   already).
;;; 2008.03.25 Dan
;;;             : * Added the new !mv-bind! option for productions.  It works
;;;             :   like !bind! except that it can bind multiple values in one
;;;             :   call.  Here is an example:
;;;             :   !mv-bind! (=var1 =var2) (some-function)
;;;             :   that would bind the variables =var1 and =var2 to the first
;;;             :   and second values returned by some-function.  If there are
;;;             :   not as many return values as variables to be bound the extra
;;;             :   are bound to nil which means the production will not match 
;;;             :   if it's a LHS !mv-bind!.
;;;             : * Fixed a bug that would let one rebind a variable with an
;;;             :   explicit !bind! on the RHS of a production.
;;; 2008.03.28 Dan
;;;             : * Changed the order of the conditions so that the slot tests
;;;             :   actually precede the variablized slot bindings.  Doesn't
;;;             :   affect the matching but it does clean up the whynot info.
;;;             : * Fixed !mv-bind! for the RHS.
;;; 2008.06.20 Dan
;;;             : * Removed the -fct from slot-specs-to-chunk-spec-list since
;;;             :   the macro was removed from the chunk-spec code.
;;; 2008.07.09 Dan
;;;             : * Patched the p*-fct to not error when there are bad lhs
;;;             :   conditions.
;;; 2008.11.03 Dan
;;;             : * Updated the parsing of a p* so that the LHS code uses
;;;             :   the new cr-buffer-read option in the conflict resolution
;;;             :   code.
;;; 2008.11.10 Dan
;;;             : * Fixed the bug introduced on 08/07/09 that was fixed for
;;;             :   the p command a while ago.
;;; 2008.11.25 Dan
;;;             : * Changed p*-fct to use add-production and remove-production
;;;             :   to better abstract away from the internals of the module.
;;;             : * Changed calls to get-production to use get-production-internal
;;;             :   to avoid the unnecessary hash-table look-up when possible.
;;; 2008.12.08 Dan [1.1]
;;;             : * Significant overhaul of the internal representation of 
;;;             :   productions.  No longer create lambdas for the conditions.  
;;;             :   Instead, create more specific single condition tests.
;;; 2009.02.10 Dan
;;;             : * Changed the module modification requests so that they don't
;;;             :   verify things before passing it along to the module.  That
;;;             :   allows a module to perform chunk-type extensions in the 
;;;             :   requests.
;;; 2009.05.04 Dan
;;;             : * Fixed an issue with symbols in variablized slots on the LHS
;;;             :   not being defined as chunks when necessary.
;;; 2009.05.05 Dan
;;;             : * Changed the inequality tests so that they all use the "right"
;;;             :   test instead of only testing > and negating and then add the
;;;             :   opposite test to the implicit ones for the tree.
;;; 2009.05.22 Dan
;;;             : * Fixed a bug in production parsing which would allow something
;;;             :   like this to "work" (p* name =goal> free ==> ...).
;;; 2009.06.03 Dan 
;;;             : * Added explicit conditions for the inequality tests to check
;;;             :   if a value is a number and an implicit check if a slot has
;;;             :   a constant to possibly help in building the conflict tree.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;; p* and p*-fct which work like p and p-fct but allow one to use variables
;;; in the place of slot names as long as those variables get bound elsewhere.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; There is only allowed to be one level of indirection, either within or
;;; across buffers.
;;;
;;; Thus, this is acceptable:
;;; 
;;; =goal>
;;;   isa goal
;;;   slot1 =slot-name
;;;   =slot-name  =next-slot
;;;   =next-slot  =some-value
;;;
;;; as is this:
;;;
;;; =goal>
;;;   isa goal
;;;   slot1 =slot-name
;;;   =slot2-name =slot-val
;;; =retrieval>
;;;   isa memory
;;;   =slot-name  =val2
;;;   slot =slot2-name
;;;
;;; but this is NOT allowed:
;;; 
;;; =goal>
;;;   isa goal
;;;   slot1 =slot-name
;;;   =slot-name   =next-slot
;;;   =next-slot   =some-value
;;;   =some-value  =value
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)



#|


General idea is that instead of a 4 pass system like p has:

test all constant slot/values

bind variablized slot values

perform explicit binds

test queries, call evals and test remaining slot specs



it's 5 passes:

check constant slots and constant values

bind variablized slot values in constant slots

bind variablized slot values in variablized slots

perform explicit binds

test queries, call evals and test remaining slot specs

|#


(defmacro define-p* (&rest definition)
  "Production definition."
  `(p*-fct ',definition))

(defun define-p*-fct (definition)
  (p*-fct definition))

(defmacro p* (&rest definition)
  "Production definition."
  `(p*-fct ',definition))


(defun p*-fct (definition)
  (let ((prod (get-module procedural)))  
    (if (procedural-p prod)  
        (let ((production (make-production :text (copy-tree definition) :dynamic t)))
          
          (unless (symbolp (car definition))
            (print-warning "Production name must be a symbol.")
            (print-warning "No production defined for ~S." definition)
            (return-from p*-fct nil))
          
          (setf (production-name production) (pop definition))
          
          (when (stringp (car definition))
            (setf (production-documentation production) (pop definition)))
          
          (aif (position '==> definition)
               (let* ((pre-lhs (parse-conditions* (subseq definition 0 it)))
                      (lhs (if (eq :error pre-lhs) :error (sort-for-binding pre-lhs)))
                      (rhs (unless (eq lhs :error)
                             (parse-actions* (subseq definition (1+ it)) lhs))))
                 (when (or (eq lhs :error)
                           (eq rhs :error))
                   (print-warning "No production defined for ~s." (production-text production))
                   (return-from p*-fct nil))
                 
                 
                 (setf (production-lhs production) lhs)
                 (setf (production-rhs production) rhs)
                 
                 ;(pprint lhs)
                 ;(pprint rhs)
                 
                 (setf (production-lhs-buffers production)
                   (remove-duplicates (mapcan #'(lambda (x)
                                                  (when (eq (caar x) #\=)
                                                    (list (cdar x))))
                                        lhs)))
                 (setf (production-rhs-buffers production)
                   (remove-duplicates (mapcan #'(lambda (x)
                                                  (unless (eq (caar x) #\!)
                                                    (list (cdar x))))
                                        rhs)))
                 
                 
                 (dolist (x (production-lhs-buffers production))
                   (unless (assoc x (procedural-buffer-indices prod))
                     (push (cons x (procedural-buffer-lookup-size prod)) (procedural-buffer-indices prod))
                     (incf (procedural-buffer-lookup-size prod))))                            
                 
                 (dolist (x (production-rhs-buffers production))
                   (unless (assoc x (procedural-buffer-indices prod))
                     (push (cons x (procedural-buffer-lookup-size prod)) (procedural-buffer-indices prod))
                     (incf (procedural-buffer-lookup-size prod))))
                             
                 ;(pprint (production-lhs-buffers production))
                 
                 (let ((variables (mapcan 'find-variables
                                    (mapcar 'second (append lhs rhs))))
                       (lhs-variables (mapcan 'find-variables (mapcar 'second lhs)))
                       (slot-variables nil)
                       (safe-bindings nil))
                   
                   (setf (production-variables production)
                     (remove-duplicates 
                      (append variables
                              (mapcar #'(lambda (x)
                                          (intern (concatenate 'string "=" (symbol-name x))))
                                (production-lhs-buffers production)))))
                   
                    
                   (setf (production-bindings production) variables)
                   
                   
                   (let ((constants nil)
                         (vars nil)
                         (var2s nil)
                         (others nil)
                         (binds nil)
                         (selection nil)
                         (implicit nil))
                   
                     
                     (dolist (buffer-name (production-lhs-buffers production))
                       (let ((bn buffer-name)  ;; closure voodoo
                             (bi (cdr (assoc buffer-name (procedural-buffer-indices prod))))
                             (bv (intern (concatenate 'string "=" (symbol-name buffer-name)))))
                         (setf (production-bindings production)
                           (remove bv (production-bindings production)))
                         
                         
                         (push (make-cr-condition :type 'bind-buffer :buffer bn :bi bi :value bv) vars)))
                     
                     (dolist (cond lhs)
                       (let* ((c cond)  ;; so the closures bind correctly
                              (buffer (cdar c))
                              (bi (cdr (assoc buffer (procedural-buffer-indices prod)))))
                         (case (caar c)
                           (#\=
                            (let* ((constant (third c))
                                   (ct (chunk-spec-chunk-type constant))
                                   (var-spec-list (fourth c))
                                   (var2-spec-list (eighth c))
                                   (others-spec-list (sixth c))
                                   (slot-vars (seventh c)))
                              
                              (setf slot-variables (append slot-vars slot-variables))
                              
                              (push-last (make-cr-condition :type 'isa :buffer buffer :bi bi :value ct) constants)
                              
                              (dolist (type (cdr (chunk-type-supertypes-fct ct)))
                                (push (make-cr-condition :type 'isa :buffer buffer :bi bi :value type) implicit))
                              
                              (push-last (list 'buffer-read buffer) selection)
                             
                              
                              (dolist (spec (chunk-spec-slot-spec constant))
                                
                                (setf others-spec-list (remove spec others-spec-list :test 'equal))
                                
                                (case (car spec)
                                  
                                  (= 
                                   ;; add the slot value for the "wide" test
                                   (push-last (make-cr-condition :type 'slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec))) constants)
                                   ;; note that the slot should be full or empty for the implicit tests if needed
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result (if (third spec) nil t)) implicit))
                                  
                                  (> 
                                   ;; add the specific test
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe> :result t) constants)
                                   ;; Explicitly this must be a number
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result t) constants)
                                   ;; implicitly that means it's not <=
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe<= :result nil) implicit)
                                   ;; note that the slot should be full for the implicit tests if needed
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                  
                                  (< ;(push (list 'slot buffer bi (second spec)  #'safe>= (third spec) nil) constants))
                                   ;; add the specific test
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe< :result t) constants)
                                   ;; Explicitly this must be a number
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result t) constants)
                                   ;; implicitly that means it's not >=
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe>= :result nil) implicit)
                                   ;; note that the slot should be full for the implicit tests if needed
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                  
                                  (>= ;(push (list 'slot buffer bi (second spec)  #'safe>= (third spec) t) constants))
                                   ;; add the specific test
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe>= :result t) constants)
                                   ;; Explicitly this must be a number
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result t) constants)
                                   ;; implicitly that means it's not <
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe< :result nil) implicit)
                                   ;; note that the slot should be full for the implicit tests if needed
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                  
                                  (<= ;(push (list 'slot buffer bi (second spec)  #'safe> (third spec) nil) constants))
                                   ;; add the specific test
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe<= :result t) constants)
                                   ;; Explicitly this must be a number
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result t) constants)
                                   ;; implicitly that means it's not >
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (if (numberp (third spec)) (third spec) nil) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe> :result nil) implicit)
                                   ;; note that the slot should be full for the implicit tests if needed
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                  
                                  
                                  (- 
                                   ;; negation test is on the others
                                   (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) others)
                                   ;; if it's '- <slot> nil' then that's got an implicit test the slot must be full
                                   (when (null (third spec))
                                     (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                  )))
                              
                              
                              (dolist (v var-spec-list)
                                (let ((var (third v)))
                                  (when (find var (production-bindings production))
                                    (setf (production-bindings production)
                                      (remove var (production-bindings production)))
                                    
                                    (setf others-spec-list (remove v others-spec-list :test 'equal))
                                    
                                    (push var safe-bindings)
                                    
                                    (let ((binding-slot (second v)))
                                      (push (make-cr-condition :type 'bind-slot :buffer buffer :bi bi :slot binding-slot :si (get-slot-index ct binding-slot) :value var) vars)
                                      
                                      ;; if there isn't already an explicit test for it not being nil 
                                      ;; add one 
                                      (unless (find (list buffer binding-slot) constants :test #'equal 
                                                    :key (lambda (x) 
                                                           (when (or (and (eq (cr-condition-type x) 'slot) ;; slot with a non-nil value
                                                                          (cr-condition-value x))
                                                                     (and (eq (cr-condition-type x) 'test-slot) ;; test-slot with either inequality test or not equal nil
                                                                          (or (not (eq (cr-condition-test x) 'chunk-slot-equal))
                                                                              (and (null (cr-condition-value x)) (null (cr-condition-result x)))
                                                                              (and (cr-condition-value x) (cr-condition-result x)))))
                                                                          
                                                             (list (cr-condition-buffer x) (cr-condition-slot x)))))
                                                    
                                                    (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot binding-slot :si (get-slot-index ct binding-slot) :result nil) constants))))))
                              
                              
                              (dolist (v var2-spec-list)
                                (when (chunk-spec-variable-p (third v))
                                  (let ((var (third v)))
                                    (when (find var (production-bindings production))
                                      (setf (production-bindings production)
                                        (remove var (production-bindings production)))
                                      
                                      (setf others-spec-list (remove v others-spec-list :test 'equal))
                                   
                                      (let ((binding-slot (second v)))
                                        
                                        (push (make-cr-condition :type 'bind-var-slot :buffer buffer :bi bi :slot binding-slot :value var) var2s))))))
                              
                              (dolist (spec others-spec-list) ;; slots that contained variables
                                (if (chunk-spec-variable-p (second spec))
                                    (case (car spec)  ;; nothing implicit is known nor is a slot index available
                                  
                                      (= 
                                       ;; add the slot value for the "wide" test
                                       (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'chunk-slot-equal :result t) others)
                                       )
                                      
                                      (> 
                                       ;; add the specific test
                                       (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe> :result t) others)
                                       )
                                      
                                      (< ;(push (list 'slot buffer bi (second spec)  #'safe>= (third spec) nil) constants))
                                       ;; add the specific test
                                       (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe< :result t) others)
                                       )
                                      
                                      (>= ;(push (list 'slot buffer bi (second spec)  #'safe>= (third spec) t) constants))
                                       ;; add the specific test
                                       (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe>= :result t) others)
                                       )
                                      
                                      (<= ;(push (list 'slot buffer bi (second spec)  #'safe> (third spec) nil) constants))
                                       ;; add the specific test
                                       (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :test 'safe<= :result t) others)
                                       )
                                      
                                      (- 
                                       ;; negation test with a variable gives no implicit info
                                       (push-last (make-cr-condition :type 'test-var-slot :buffer buffer :bi bi :value (third spec) :test 'chunk-slot-equal :slot (second spec) :result nil) others)
                                       ))
                                  
                                  
                                  (case (car spec) ; treat it just like a normal
                                    
                                    (= 
                                     ;; add the slot value for the "wide" test
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'chunk-slot-equal :result t) others)
                                     ;; note that the slot should be full for the implicit tests if needed
                                     (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                    
                                    (> 
                                     ;; add the specific test
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe> :result t) others)
                                     ;; note that the slot should be full for the implicit tests if needed
                                     (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                    
                                    (< ;(push (list 'slot buffer bi (second spec)  #'safe>= (third spec) nil) constants))
                                     ;; add the specific test
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe< :result t) others)
                                     ;; note that the slot should be full for the implicit tests if needed
                                     (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                    
                                    (>= ;(push (list 'slot buffer bi (second spec)  #'safe>= (third spec) t) constants))
                                     ;; add the specific test
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe>= :result t) others)
                                     ;; note that the slot should be full for the implicit tests if needed
                                     (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                    
                                    (<= ;(push (list 'slot buffer bi (second spec)  #'safe> (third spec) nil) constants))
                                     ;; add the specific test
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :slot (second spec) :si (get-slot-index ct (second spec)) :test 'safe<= :result t) others)
                                     ;; note that the slot should be full for the implicit tests if needed
                                     (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) implicit))
                                                                        
                                    (- 
                                     ;; negation test with a variable gives no implicit info
                                     (push-last (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value (third spec) :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result nil) others)
                                     ))))
                                
                              
                              
                              
                              
                              ))
                         (#\?
                          (let ((constant-queries (third c))
                                (variable-queries (fourth c)))
                            
                            ;(pprint constant-queries)
                            ;(pprint variable-queries)
                                                       (dolist (q constant-queries)
                              (push-last (make-cr-condition :type 'query :buffer buffer :slot (second q) :value (third q) :result (eq '= (first q))) constants))
                            
                            (dolist (q variable-queries)
                              (push-last (make-cr-condition :type 'query :buffer buffer :slot (second q) :value (third q) :result (eq '= (first q))) others))
                            
                            (push-last
                             (list 'query-buffer buffer                             
                                   (mapcar #'(lambda (x)
                                               (cons (second x) (third x)))
                                     (append constant-queries variable-queries)))
                             selection)))
                           (t
                           (case (cdar c)
                             ((eval safe-eval)
                              (push-last (make-cr-condition :type 'eval :value (car (second c))) others)
                               )
                             
                             
                             ((bind safe-bind)
                               
                               (unless (find (car (second c)) (production-bindings production))
                                 (print-warning "Cannot have two explicit bindings for variable ~S" (car (second c)))
                                 (print-warning "No production defined for ~s." (production-text production))
                                 (return-from p*-fct nil))
                               
                               (setf (production-bindings production)
                                 (remove (car (second c))
                                         (production-bindings production)))
                               (push (cons
                                      (cons (car (second c)) (find-variables (second (second c))))
                                      (make-cr-condition :type 'bind :value (car (second c)) :result (second (second c))))
                                     binds))
                             ((mv-bind)
                              
                              (let ((bind-vars (car (second c))))
                                 (dolist (x bind-vars)
                                   (unless (find x (production-bindings production))
                                     (print-warning "Cannot have two explicit bindings for variable ~S" x)
                                     (print-warning "No production defined for ~s." (production-text production))
                                     
                                     (return-from p*-fct nil)))
                                 
                                   (dolist (x bind-vars)
                                    (setf (production-bindings production)
                                      (remove x (production-bindings production))))
                                 
                                 (push (cons
                                        (cons bind-vars (find-variables (second (second c))))
                                        (make-cr-condition :type 'mv-bind :value bind-vars :result (second (second c))))
                                       binds))))))))
                     
                     
                     ;;; Make sure everything necessary is bound
                     
                     (awhen (some (lambda (x) (find x (production-bindings production))) lhs-variables)
                            (print-warning "No production defined for ~s because ~s is not bound on the LHS." (production-text production) it)
                            (return-from p*-fct nil))
                     
                     
                     ;;; Make sure that none of the slot name variables are bound in an unsafe manner
                     
                     (awhen (find-if-not (lambda (x) (find x safe-bindings)) slot-variables)
                            (print-warning "No production defined for ~s because slot-name variable ~s is not bound in a constant named slot i.e. there is more than one level of indirection." (production-text production) it)
                            (return-from p*-fct nil))
                            
                     
                     ;;; Check the explicit bindings for circularities
                     
                     (when (circular-references binds)
                       (print-warning "No production defined for ~s because there are circular references in the explicit LHS !bind!'s." (production-text production))
                       (return-from p*-fct nil))
                     
                     (setf binds (mapcar #'cdr (sort binds #'bind-order :key #'car)))
                   
                     ;;; build the whole conditions list
                     
                      
                     (setf (production-constants production) constants)
                     (setf (production-binds production) (append vars var2s binds))
                     (setf (production-others production) others)
                     (setf (production-selection-code production) selection)
                     (setf (production-implicit production) implicit)
                    )
                   
                   ;(pprint (production-conditions production))
                   )                  
                 
                 
                 (dolist (action rhs)
                   (let ((a action))
                     (awhen (some #'(lambda (x)
                                      (find x (production-bindings production)))
                                  (find-variables  (if (or (equal (car a) (cons #\! 'bind))
                                                           (equal (car a) (cons #\! 'safe-bind))
                                                           (equal (car a) (cons #\! 'mv-bind)))
                                                       (progn  (cdr (second a)))
                                                     (progn  (second a)))))
                          
                          (print-warning "Unbound variable ~s on RHS in ~s." it (second a))
                          (print-warning "No production defined for ~s." (production-text production))
                            
                            (return-from p*-fct nil))
                   
                   (case (caar a)
                     (#\=
                      (cond ((null (second a))
                             ;; That's a dummy action to keep the
                             ;; buffer alive
                             )
                            ((= (length (second a)) 1)
                             ;; an overwrite
                             
                             (push-last 
                              (list #'(lambda () 
                                        (schedule-overwrite-buffer-chunk (cdar a)
                                                                         (replace-variables (first (second a))
                                                                                            (production-bindings production))
                                                                         0
                                                                         :module 'procedural
                                                                         :priority 90
                                                                         :output (procedural-rhst prod)))
                                    a)
                              (production-actions production)))
                            (t
                             ;; a true buffer modification
                             (push-last 
                              (list #'(lambda () 
                                        (let ((expansion (replace-variables (second a) (production-bindings production)))
                                              (buffer (cdar a)))
                                          (extend-buffer-chunk buffer expansion)
                                          (schedule-mod-buffer-chunk buffer
                                                                     expansion
                                                                     0
                                                                     :module 'procedural
                                                                     :priority 100
                                                                     :output (procedural-rhst prod))))
                                    a)
                              (production-actions production)))))
                        
                     (#\-
                      (push-last 
                       (list #'(lambda () 
                                 (schedule-clear-buffer (cdar a)
                                                        0
                                                        :module 'procedural
                                                        :priority 10
                                                        :output (when (procedural-rhst prod) 'medium)))
                             a)
                       (production-actions production)))
                     
                     (#\+
                      (cond ((eq (car (second a)) 'isa)
                             ;; a full request
                             
                             (push-last 
                              (list #'(lambda () 
                                        (schedule-module-request (cdar a)
                                                                 (define-chunk-spec-fct 
                                                                     (replace-variables (second a) (production-bindings production)))
                                                                 0
                                                                 :module 'procedural
                                                                 :priority 50
                                                                 :output (procedural-rhst prod)))
                                    a)
                              (production-actions production)))
                            ((= (length (second a)) 1)
                             ;; a direct request
                             
                             (push-last 
                              (list #'(lambda () 
                                        
                                        (schedule-event-relative 0 #'(lambda () (schedule-module-request (cdar a)
                                                                                                         (chunk-name-to-chunk-spec 
                                                                                                          (car (replace-variables (second a)
                                                                                                                                  (production-bindings production))))
                                                                                                         0
                                                                                                         :module 'procedural
                                                                                                         :priority 50
                                                                                                         :output (procedural-rhst prod)))
                                                                 :module 'procedural
                                                                 :priority 99
                                                                 :output nil)
                                        
                                        )
                                    a)
                              (production-actions production)))
                            
                            (t
                             ;; a buffer modification request
                             (push-last 
                              (list #'(lambda () 
                                        (schedule-module-mod-request (cdar a)
                                                                     (replace-variables (second a)
                                                                                        (production-bindings production))
                                                                     0
                                                                     :verify nil
                                                                     :module 'procedural
                                                                     :priority 50
                                                                     :output (procedural-rhst prod)))
                                    a)
                              (production-actions production)))))
                     
                     (t
                      (case (cdar a)
                        ((eval safe-eval)
                         (push-last (list #'(lambda ()
                                              (eval (replace-variables-for-eval (car (second a))
                                                                       (production-bindings production))))
                                          a)
                                    (production-actions production)))
                        
                        ((bind safe-bind)
                         (setf (production-bindings production)
                           (remove (car (second a)) (production-bindings production)))
                         (push-last (list #' (lambda ()
                                               (bind-variable (car (second a))
                                                              (eval (replace-variables-for-eval (second (second a))
                                                                                       (production-bindings production)))
                                                              production))
                                          a)
                                    (production-actions production)))
                        
                        ((mv-bind)
                         (let ((bind-vars (car (second a))))
                           (dolist (x bind-vars)
                             (unless (find x (production-bindings production))
                               (print-warning "Cannot have two explicit bindings for variable ~S" x)
                               (print-warning "No production defined for ~s." (production-text production))
                               
                               (return-from p*-fct nil)))
                           
                           (dolist (x bind-vars)
                             (setf (production-bindings production)
                               (remove x (production-bindings production))))
                           
                           (push-last (list 
                                       #'(lambda ()
                                           (let ((vals (multiple-value-list 
                                                        (eval (replace-variables-for-eval (second (second a)) (production-bindings production))))))
                                               (dolist (x bind-vars)
                                                 (bind-variable x (if vals (pop vals) nil) production))))
                                       a)
                                      (production-actions production))))
                        
                        (output
                         (push-last (list #'(lambda ()
                                              (print-production-output 
                                               (second a)
                                               (replace-variables (second a)
                                                                  (production-bindings production))))
                                          a)
                                    (production-actions production)))
                        (stop
                         (push-last (list #'(lambda ()
                                              (schedule-break-relative 0 :priority :min
                                                                       :details "Stopped by !stop!"))
                                          a)
                                    (production-actions production))))))))
                 
                 ;;; Add the implicit clears
                 
                 (dolist (y lhs)
                   (let ((x y))
                     (when (eql #\= (caar x))
                     (unless (or (find (cdar x) (procedural-unharvested-buffers prod))
                                 (find (cdar x) rhs :key #'cdar))
                       
                       (push-last 
                        (list #'(lambda () 
                                  (schedule-clear-buffer (cdar x)
                                                         0
                                                         :module 'procedural
                                                         :priority 10
                                                         :output (when (procedural-rhst prod) 'medium)))
                              (list 'implicitly 'clear (cdar x)))
                        (production-actions production))))))
                 
                 (dolist (y rhs)
                   (let ((x y))
                     (when (and (eql #\+ (caar x)) 
                                (or
                                 (eq 'isa (car (second x)))
                                 (= (length (second x)) 1))
                                (not (find (cons #\- (cdar x)) rhs :key #'car :test #'equal)))
                       
                       (push-last 
                        (list #'(lambda () 
                                  (schedule-clear-buffer (cdar x)
                                                         0
                                                         :module 'procedural
                                                         :priority 10
                                                         :output (when (procedural-rhst prod) 'medium)))
                              (list 'clear 'on 'request (cdar x)))
                        (production-actions production))))) 
                       
                 ;;; Parse LHS for unknown chunks
                 
                 (dolist (x lhs)
                   (cond ((eql #\= (caar x))
                          (dolist (slot (fifth x))
                            (unless (or (chunk-spec-variable-p (third slot))
                                        (chunk-p-fct (third slot))
                                        (stringp (third slot))
                                        (listp (third slot))
                                        (numberp (third slot))
                                        (eq t (third slot)))
                              (create-undefined-chunk (third slot)))))
                         ((eql #\? (caar x))
                          (dolist (slot (third x))
                            (unless (or (chunk-spec-variable-p (third slot))
                                        (chunk-p-fct (third slot))
                                        (numberp (third slot))
                                        (stringp (third slot))
                                        (listp (third slot))
                                        (numberp (third slot))
                                        (eq t (third slot)))
                              (create-undefined-chunk (third slot)))))))
                 
                 ;;; Parse RHS for unknown chunks
                 ;;; Only in = modifications and direct requests though
                 ;;; anything in a full + is up to the module to
                 ;;; handle i.e. this is where I see something
                 ;;; like retrieval variables being not an =
                 ;;; working in about 2 minutes...
                 
                 (dolist (x rhs)
                   (cond ((eql #\= (caar x))
                          (if (= (length (second x)) 1)
                              (cond ((or (chunk-spec-variable-p (car (second x)))
                                         (chunk-p-fct (car (second x))))
                                     ;;; nothing because that's safe
                                     )
                                    ((symbolp (car (second x)))
                                     (create-undefined-chunk (car (second x))))
                                    (t
                                     (print-warning "No production defined for ~s because ~s is not a variable or chunk name in a buffer overwrite action." (production-text production) (car (second x)))
                                     (return-from p*-fct nil)))
                            (dolist (val (mapcar #'cdr (query-list-to-conses (second x)))) ;; I know it's a slot-value pair list
                              
                              (unless (or (chunk-spec-variable-p val)
                                          (chunk-p-fct val)
                                          (numberp val)
                                          (stringp val)
                                          (listp val)
                                          (numberp val)
                                          (eq t val))
                                (create-undefined-chunk val)))))
                         ((eql #\+ (caar x))
                          (when (= (length (second x)) 1) ;; don't create chunks in full requests
                            (cond ((or (chunk-spec-variable-p (car (second x)))
                                         (chunk-p-fct (car (second x))))
                                     ;;; nothing because that's safe
                                     )
                                    ((symbolp (car (second x)))
                                     (create-undefined-chunk (car (second x))))
                                    (t
                                     (print-warning "No production defined for ~s because ~s is not a variable or chunk name in a direct request action." (production-text production) (car (second x)))
                                     (return-from p*-fct nil)))))))
                  
                 ;; Replace the special case for vision and
                 ;; use the warning mechanism that's available now
                 
                 (dolist (x rhs)
                   (let ((y x))
                     (when (and (eql #\+ (caar x))
                                (or (= (length (second x)) 1) ;; a direct request
                                    (eq (car (second x)) 'isa)) ;; a full request
                                (require-module-warning? (cdar x)))
                       (if (= (length (second x)) 1)
                           
                           (push #'(lambda ()
                                     (module-warning (cdar y) (chunk-chunk-type-fct (car (replace-variables (second y) (production-bindings production))))))
                                 (production-conflict-code production))
                         (push #'(lambda ()
                                   (module-warning (cdar y) (second (second y))))
                               (production-conflict-code production))))))
                 
                 ;(pprint (production-actions production))
               
               (awhen (get-production-internal (production-name production) prod)
                 (print-warning "Production ~S already exists and it is being redefined." (production-name production))
                 (remove-production it prod))
               
               (add-production production prod) 
               
               ;; If a new production is created and conflict resolution 
               ;; is waiting put it back on the queue
               
               (un-delay-conflict-resolution)
               
               (production-name production))
          (progn
            (print-warning "Production is missing the ==> separator.")
            (print-warning "No production defined for ~s." (production-text production)))))
    (print-warning "No procedural modulue found cannot create production."))))



(defun replace-slot-variables (arg bindings)
  (cond ((listp arg) 
         (mapcar #'(lambda (x)
                     (replace-slot-variables x bindings))
           arg))
        ((chunk-spec-variable-p arg) 
         (aif (assoc arg bindings)
              (if (cdr it) (cdr it) arg)
              arg))
        (t arg)))



(defun find-variables-in-variable-slots (spec-list)
  (do ((vals nil)
       (specs (cddr spec-list)
              (cdddr specs))
       )
      ((null specs) vals)
    
    (when (and (eq '= (car specs))
               (chunk-spec-variable-p (third specs)))
      (push (third specs) vals))))


(defun define-variable-chunk-spec-fct (specifications-list)
  "Allows variables in the slot-name position, but the return value isn't
   really a valid chunk-spec for purposes of testing chunks"
  (verify-current-mp  
   "define-variable-chunk-spec-fct called with no current meta-process."
   (verify-current-model
    "define-variable-chunk-spec-fct called with no current model."
    (cond ((null specifications-list)
           (print-warning "No specification in call to define-chunk-spec."))
          ((= (length specifications-list) 1)
           (if (get-chunk (car specifications-list))
               (chunk-name-to-chunk-spec (car specifications-list))
             (print-warning 
              "define-chunk-spec's 1 parameter doesn't name a chunk: ~S" 
              specifications-list)))
          
          ((not (eq (car specifications-list) 'isa))
           (print-warning 
            "First element to define-chunk-spec isn't the symbol ISA. ~s" 
            specifications-list))
          ((not (get-chunk-type (second specifications-list)))
           (print-warning 
            "Second element in define-chunk-spec isn't a chunk-type. ~S" 
            specifications-list))
          (t
           (let* ((ct (get-chunk-type (second specifications-list)))
                  (new-spec (make-act-r-chunk-spec :type ct))
                  (slots (process-variable-slots-specs ct (cddr specifications-list))))
             (unless (eq slots :error)
               (setf (act-r-chunk-spec-slots new-spec) slots)
               new-spec)))))))


(defun process-variable-slots-specs (chunk-type specs)
  (let ((slots nil))
    (loop 
      (when (null specs)
        (return slots))
      (let ((spec (make-act-r-slot-spec)))
        (when (find (car specs) '(= - > < >= <=))
          (setf (act-r-slot-spec-modifier spec) (pop specs)))
        (when (null specs)
          (print-warning 
           "Invalid specs in call to define-chunk-spec - not enough arguments")
          (return :error))
        (unless (or (chunk-spec-variable-p (car specs)) ;; let this go through...
                    (valid-slot-name (car specs) chunk-type) 
                    (keywordp (car specs)))
          (print-warning "Invalid slot-name ~S in call to define-chunk-spec." 
                         (car specs))
          (return :error))
        (setf (act-r-slot-spec-name spec) (pop specs))
        (when (null specs)
          (print-warning 
           "Invalid specs in call to define-chunk-spec - not enough arguments")
          (return :error))
        (setf (act-r-slot-spec-value spec) (pop specs))
        (push spec slots)))))


(defun parse-conditions* (definition)
  (let ((segments (segment-production definition)))
    (if (eq segments :error)
        (progn
          (print-warning "First item on LHS is not a valid command")
          :error)
      (do* ((segs segments (cdr segs))
            (seg (car segs) (car segs))
            (cmd (parse-command (car seg) :operators '(#\? #\=)
                                :commands '(eval safe-eval bind safe-bind mv-bind)) 
                 (parse-command (car seg) :operators '(#\? #\=)
                                :commands '(eval safe-eval bind safe-bind mv-bind)))
            (conditions nil))
            
           ((null seg) conditions)
               
        (case (car cmd)
          (#\=
           (if (< (length (cdr seg)) 2)
               (progn
                 (print-warning "Missing chunk-type test in condition for ~s buffer." (cdr cmd))
                 (return-from parse-conditions* :error))
             (aif (define-variable-chunk-spec-fct (cdr seg))
                  (progn
                    (when (some #'keywordp (chunk-spec-slots it))
                      (print-warning "Request parameters not allowed in buffer tests: ~s" seg)
                      (return-from parse-conditions* :error))
                    (let* ((slot-specs (chunk-spec-slot-spec it))
                           (constants)
                           (variables)
                           (var2)
                           (others)
                           (slot-vars))
                      
                      (dolist (spec slot-specs)
                        
                        (when (chunk-spec-variable-p (second spec))
                          (push (second spec) slot-vars))
                        
                        
                        (cond ((not (eq '= (first spec)))
                               (push-last spec others))
                              ((chunk-spec-variable-p (second spec))
                               (push-last spec var2)
                               (push-last spec others))
                              ((chunk-spec-variable-p (third spec))
                               (push-last spec variables)
                               (push-last spec others))
                              (t
                               (push-last spec constants))))
                      
                      
                      
                      (push-last (list cmd (cdr seg) 
                                       (define-chunk-spec-fct (slot-specs-to-chunk-spec-list (chunk-spec-chunk-type it) constants))
                                       variables
                                       slot-specs 
                                       others ;(when others (slot-specs-to-chunk-spec-list (chunk-spec-chunk-type it) others))
                                       slot-vars
                                       var2)
                                 conditions)))
                  (progn
                    (print-warning "Invalid syntax in ~s condition." (car seg))
                    (return-from parse-conditions* :error)))))
          (#\?
           (let ((queries (process-query-specs (cdr cmd) (cdr seg))))
             
             (cond ((eq queries :error)
                    (print-warning "Invalid buffer query ~S." seg)
                    (return-from parse-conditions* :error))
                   
                   (t
                    (let ((constant-queries (remove-if #'chunk-spec-variable-p queries :key #'third))
                          (variable-queries (remove-if-not #'chunk-spec-variable-p queries :key #'third)))
                      (push-last (list cmd (cdr seg) constant-queries variable-queries) conditions))))))
          (t
           (case (cdr cmd)
             ((bind safe-bind)
              (if (and (= (length seg) 3)
                       (chunk-spec-variable-p (second seg)))
                  (push-last (list cmd (cdr seg)) conditions)
                (progn
                  (print-warning "Invalid bind command: ~s" seg)
                  (return-from parse-conditions* :error))))
             ((mv-bind)
              (if (and (= (length seg) 3)
                       (listp (second seg))
                       (every 'chunk-spec-variable-p (second seg)))
                  (push-last (list cmd (cdr seg)) conditions)
                (progn
                  (print-warning "Invalid mv-bind command: ~s" seg)
                  (return-from parse-conditions* :error))))
             ((eval safe-eval)
              (if (= (length seg) 2)
                  (push-last (list cmd (cdr seg)) conditions)
                (progn
                  (print-warning "Invalid eval command: ~s" seg)
                  (return-from parse-conditions* :error))))
             (t
              (print-warning "Invalid command: ~s" seg)
              (return-from parse-conditions* :error)))))))))


(defun valid-variable-chunk-mod-spec (chunk-type modifications-list)
  (if (oddp (length modifications-list))
      (print-warning "Odd length modifications list.")
    (if (procedural-check-p*-mods (get-module procedural))
      (do ((slots nil (cons (car s) slots))
           (s modifications-list (cddr s)))
          ((null s) 
           (and (every #'(lambda (slot)
                           (or (chunk-spec-variable-p slot)
                               (valid-slot-name slot (get-chunk-type chunk-type))))
                       slots)
                (= (length slots) (length (remove-duplicates slots))))))
      t)))
  
(defun parse-actions* (definition conditions)
  (let ((segments (segment-production definition)))
    (if (eq segments :error)
        (progn
          (print-warning "First item on RHS is not a valid command")
          :error)
      (do* ((segs segments (cdr segs))
            (seg (car segs) (car segs))
            (cmd (parse-command (car seg) :operators '(#\- #\+  #\=)) 
                 (parse-command (car seg) :operators '(#\- #\+  #\=)))
            (actions nil))
           
           ((null seg) actions)
        (when (null cmd)
          (print-warning "Invalid command on RHS: ~S" (car seg))
          (return-from parse-actions* :error))
        
        (case (car cmd)
          (#\-
           (if (= (length seg) 1)
               (push-last (list cmd) actions)
             (progn
               (print-warning "Invalid - buffer command: ~s" seg)
               (return-from parse-actions* :error))))
          
          (#\=
           (cond ((= (length seg) 1)
                  (push-last (list cmd) actions))
                 ((= (length seg) 2)
                  (push-last (list cmd (cdr seg)) actions))
                 (t
                  (let* ((lhs-binding (find (cons #\= (cdr cmd)) conditions
                                           :test #'equal :key #'car))
                         (chunk-type (if lhs-binding
                                         (chunk-spec-chunk-type (third lhs-binding))
                                       nil)))
                    (if chunk-type
                        (if (valid-variable-chunk-mod-spec chunk-type (cdr seg))
                            (push-last (list cmd (cdr seg)) actions)
                          (progn
                            (print-warning "Invalid buffer modification ~s." seg)
                            (return-from parse-actions* :error)))
                      (progn
                        (print-warning "Cannot modify buffer ~s if not matched on LHS." (cdr cmd))
                        (return-from parse-actions* :error)))))))
                    
           (#\+
           (cond ((= (length seg) 1)
                  (print-warning "Buffer request ~s requires some parameters." (car seg))
                  (return-from parse-actions* :error))
                 ((= (length seg) 2)
                  (push-last (list cmd (cdr seg)) actions))
                 (t
                  (if (eq (second seg) 'isa)
                      ;; This is a full request
                      (progn
                        (aif (define-variable-chunk-spec-fct (cdr seg))
                             (progn
                               (unless (every (lambda (x)
                                                ; check for the variables
                                                (or (chunk-spec-variable-p x)
                                                    (valid-buffer-request-slot 
                                                     (cdr cmd) 
                                                     (chunk-spec-chunk-type it) x)))
                                                (chunk-spec-slots it))
                                 
                                 (print-warning  "Invalid slot value in request: ~s" seg)
                                 (return-from parse-actions* :error))
                  
                               (push-last (list cmd (cdr seg)) actions))
                             (progn
                               (print-warning "Invalid syntax in action ~s." seg)
                               (return-from parse-actions* :error))))
                    ;; otherwise it's assumed to be a modification request
                    (progn
                      (let* ((lhs-binding (find (cons #\= (cdr cmd)) conditions
                                                :test #'equal :key #'car))
                             (chunk-type (if lhs-binding
                                             (chunk-spec-chunk-type (third lhs-binding))
                                           nil)))
                        (if chunk-type
                            (if (valid-variable-chunk-mod-spec chunk-type (cdr seg))
                                (push-last (list cmd (cdr seg)) actions)
                              (progn
                                (print-warning "Invalid buffer modification ~s." seg)
                                (return-from parse-actions* :error)))
                          (progn
                            (print-warning "Cannot modify buffer ~s if not matched on LHS." 
                             (cdr cmd))
                            (return-from parse-actions* :error)))))))))
          
          (t
           (case (cdr cmd)
             ((bind safe-bind)
              (if (and (= (length seg) 3)
                       (chunk-spec-variable-p (second seg)))
                  (push-last (list cmd (cdr seg)) actions)
                (progn
                  (print-warning "Invalid bind command: ~s" seg)
                  (return-from parse-actions* :error))))
             ((mv-bind)
              (if (and (= (length seg) 3)
                       (listp (second seg))
                       (every 'chunk-spec-variable-p (second seg)))
                  (push-last (list cmd (cdr seg)) actions)
                (progn
                  (print-warning "Invalid mv-bind command: ~s" seg)
                  (return-from parse-actions* :error))))
             ((eval safe-eval output)
              (if (= (length seg) 2)
                  (push-last (list cmd (cdr seg)) actions)
                (progn
                  (print-warning "Invalid ~s command: ~s" (cdr cmd) seg)
                  (return-from parse-actions* :error))))
             (stop
              (if (= (length seg) 1)
                  (push-last (list cmd) actions)
                (progn
                  (print-warning "Invalid stop command: ~s" seg)
                  (return-from parse-actions* :error))))
             (t
              (print-warning "Invalid command: ~s" seg)
              (return-from parse-actions* :error)))))))))




(defun extend-buffer-chunk (buffer-name mod-list)
  (let ((chunk (buffer-read buffer-name)))
      (cond ((null chunk)
             (print-warning 
              "extend-buffer-chunk called with no chunk in buffer ~S"
              buffer-name))
            (t
             (let ((m-list (copy-list mod-list))
                   (new-slots nil)
                   (ct (chunk-chunk-type-fct chunk))
                   (procedural (get-module procedural)))
               (while m-list
                 (let ((new? (pop m-list)))
                   (unless (valid-chunk-type-slot ct new?)
                     (push new? new-slots))
                   (pop m-list)))
               (dolist (new-slot new-slots)
                 (extend-chunk-type-slots ct new-slot)
                 
                 (schedule-event-relative 0 'extending-chunk-type 
                                      :module 'procedural
                                      :priority 51 ; just before the modification itself
                                      :params (list ct new-slot)
                                      :output (procedural-rhst procedural))))))))


(defun extending-chunk-type (ct slot)
  "Dummy function to show chunk extension in the schedule"
  (declare (ignore ct slot)))


#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
