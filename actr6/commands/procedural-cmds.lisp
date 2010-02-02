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
;;; Filename    : procedural-cmds.lisp
;;; Version     : 1.1
;;; 
;;; Description : User functions for the procedural module.
;;; 
;;; Bugs        : * [should be fixed now]
;;;             :   Doesn't properly check that a variable has a binding before
;;;             :   use on the LHS.  Doesn't break things, but it doesn't warn
;;;             :   in the case of productions that will never fire.
;;;
;;;             : * [fixed]
;;;             :   RHS direct requests don't schedule an implicit clear because
;;;             :   the test for that is that the second item be ISA (to avoid
;;;             :   clearing on a mod-request).  SO, was that by design or an
;;;             :   over-generalization?  IF it's a bug, fix in p* as well.


;;; To do       : * [done]
;;;             :   Try redoing the matching so that all bindings occur before
;;;             :   all other tests so that buffer ordering isn't important
;;;             :   other than to determine which buffer is used for the
;;;             :   binding.
;;;             : * Save the variable name in the parse-condition (probably
;;;             :   as a third element to the first or something) so that
;;;             :   it doesn't have to be created and interned in p.
;;;             : * In general that can be done in the parsing so that 
;;;             :   there's no need to call find-variables in p.
;;;             : * Signal a warning when there is a !eval! in the slot
;;;             :   value position because a list is valid but it won't
;;;             :   do the evaling which could confuse those moving things
;;;             :   from 5.
;;;
;;;
;;; ----- History -----
;;; 2005.01.15 Dan
;;;             : * Moved things to here from procedural in core-modules.
;;; 2005.01.16 Dan
;;;             : * Added the checks for delayed conflict resolution so that
;;;             :   user changes can force it back into the queue (either a
;;;             :   new/changed production or setting a parameter with spp).
;;; 2005.01.17 Dan
;;;             : * Changed model-output to command-output in spp and its
;;;             :   associated functions.
;;; 2005.01.19 Dan
;;;             : * Added most of the ACT-R 4/5 user commands.
;;; 2005.02.03 Dan
;;;             : * Moved production-firing-only here from the printing module.
;;; 2005.02.04 Dan
;;;             : * Complete reworking of how production parsing occurs to
;;;             :   attempt to cache more stuff (define-chunk-specs) at parse
;;;             :   time.
;;;             : * Undid the 80 column limit because it really makes it hard
;;;             :   for me to work with this.
;;; 2005.02.10 Dan
;;;             : * Use the new parse tables in parse-conditions and parse-
;;;             :   action to help make a reset faster.
;;; 2005.02.13 Dan 
;;;             : * Minor change to add more #' to the lambdas.
;;; 2005.02.14 Dan
;;;             : * Fixed bind-variable so that it returns nil wnen a
;;;                 variable gets bound to nil so it breaks the matching.
;;; 2005.02.25 Dan
;;;             : * Added replace-variables-for-eval because the values passed
;;;             :   to a function in an eval need to be quoted.
;;; 2005.03.18 Dan
;;;             : * Modfying the p command so that it only does strict
;;;             :   harvesting for the buffers not on the do-not-harvest
;;;             :   list.
;;; 2005.04.12 Dan
;;;             : * Fixed a small typo (LHS->RHS) in a warning in parse-actions. 
;;; 2005.04.26 Dan
;;;             : * Restored the setting of lhs and rhs of the production to
;;;             :   the parsed code for use by compilation.
;;; 2005.05.03 Dan
;;;             : * Possible bug has shown up (see above) and may need to be
;;;             :   fixed...
;;; 2005.05.04 Dan
;;;             : * Fixed the issue with direct requests not clearing the
;;;             :   buffer.
;;; 2005.05.11 Dan
;;;             : * Changed the output parameter for buffer clearing so that
;;;             :   it prints in the medium traces as well.
;;; 2005.05.17 Dan
;;;             : * Added the delete-production function.  Not intended as a
;;;             :   user command...
;;; 2005.06.01 Dan
;;;             : * Made pp/pp-fct more tolerant of bad production names.
;;; 2005.06.02 Dan
;;;             : * Made replace-variables-for-eval smarter about replacing
;;;             :   a variable with another variable because compilation can
;;;             :   result in that and it's not desireable to have it quoted.
;;; 2005.06.18 Dan
;;;             : * Modified spp so it prints a warning if a named production
;;;             :   does not exist.
;;; 2005.09.09 Dan
;;;             : * Changed reference of chunk-to-chunk-spec to chunk-name-to-
;;;             :   chunk-spec and made sure to call it appropriately.
;;; 2005.09.14 Dan
;;;             : * Change to the priority of RHS actions. = is now set to a
;;;             :   higher priority than +.  See log in procedural.lisp for
;;;             :   the detailed rational.
;;; 2005.11.09 Dan
;;;             : * Fixed a bug in pmatches which occured when more than one
;;;             :   production matched the current state - it threw an error.
;;;             :   That also caused whynot to break because it uses pmatches.
;;; 2005.12.07 Dan
;;;             : * Restricting the potential uses of !bind!.  Instead of 
;;;             :   having it participate in bindings at the same level as the
;;;             :   buffers, it now only occurs explicitly after all buffer
;;;             :   tests have been allowed to do their bindings and warns
;;;             :   (rejecting the production) if there is an attempt to rebind
;;;             :   a particular variable.  It also now tests for circualar
;;;             :   references among !binds! and orders them as needed.
;;; 2005.12.15 Dan 
;;;             : * Modified replace-variables to handle something like
;;;             :   (cons 'a 'b) properly because I'm using that in the
;;;             :   compilation code now.  If this seems to impact performance
;;;             :   then I'll need to just make a special version for that
;;;             :   case.
;;; 2005.12.22 Dan
;;;             : * Removed the spp code to put in the utility-and-reward file.
;;; 2006.03.09 Dan
;;;             : * Get-production has changed to not require the procedural
;;;             :   module so make that change as needed.
;;; 2006.03.10 Dan
;;;             : * Changed pp to not exclude disabled productions and going to
;;;             :   make print-production mark them as such instead.
;;; 2006.03.13 Dan 
;;;             : * Fixed pmatches to deal with the new utility computations.
;;; 2006.03.14 Dan
;;;             : * Added the define-p macro which does the same thing as p,
;;;             :   but the define- name provides some consistency with every-
;;;             :   thing else (define-model, define-chunks, define-module, etc)
;;;             :   and helps people who use MCL as the editor.
;;; 2006.03.14 Dan
;;;             : * Added the function all-productions because I realized that
;;;             :   using pp is a bad idea since it invokes the cost of the
;;;             :   production printer each time. 
;;; 2006.03.21 Dan
;;;             : * Fixed a bug with un-delay-conflict-resolution that slipped
;;;             :   in when I updated the utility mechanims.
;;; 2006.10.11 Dan
;;;             : * Changed the priority of the buffer-overwrite action to be
;;;             :   90 instead of 100 (the priority of the buffer mod action)
;;;             :   because the modifications must occur first, but if they
;;;             :   have the same priority then they could be reversed which
;;;             :   can lead to run-time issues and isn't right with respect
;;;             :   to how productions should work.
;;; 2006.11.07 Dan
;;;             : * Fixed a bug in all-productions which caused errors if there
;;;             :   were more than one model defined and it was called with no
;;;             :   current model (not an issue for running models).
;;;             : * Made penable, pdisable, pbreak, and punbreak report nicer
;;;             :   warnings when there is no current model.
;;;             : * Fixed the penable macro so it actually called penable-fct!
;;;             : * Changed penable-fct so that like punbreak if it's passed 
;;;             :   nil all productions are enabled.
;;; 2006.11.08 Dan
;;;             : * Changed whynot so it better reports the warning that there
;;;             :   is no current model.
;;; 2006.11.09 Dan
;;;             : * Changed print-production-output so that it can distinguish
;;;             :   between an explicit string as the first element on a list
;;;             :   and when it just happens to be a string in the current 
;;;             :   binding (to differentiate when it should use the string
;;;             :   as a format string and when it should just output it as
;;;             :   is).
;;;             : * Modified the call to print-production-output in the parsing
;;;             :   of an !output! command so that the "old style" format string
;;;             :   usage doesn't get triggered implicitly (using the change
;;;             :   described above).
;;;             : * Fixed a bug in production parsing that would lead to run time
;;;             :   errors if a buffer overwrite action didn't specify a chunk
;;;             :   or at least a variable (the variable could still bind to a
;;;             :   non-chunk at run time).
;;; 2006.11.10 Dan
;;;             : * Additional update to the parsing like the last one - now
;;;             :   direct requests must be a variable or chunk.
;;; 2006.11.20 Dan
;;;             : * Removed the special case implicit action for visual requests in
;;;             :   the production definition and replaced it with the new 
;;;             :   (generic) module-warning mechanims.
;;; 2007.06.11 Dan
;;;             : * Fixed a bug in REPLACE-VARIABLES-FOR-EVAL which would quote
;;;             :   strings that had been bound to the variable in the replacement.
;;; 2007.06.18 Dan
;;;             : * Moved slot-specs-to-chunk-spec-list to the chunk-spec file
;;;             :   and then changed the references in here to add the -fct.
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
;;; 2008.03.27 Dan
;;;             : * Fixed a nasty bug I introduced in production parsing with
;;;             :   the sorting of conditions for binding pruposes.
;;; 2008.03.28 Dan
;;;             : * Fixed !mv-bind! for the RHS.
;;; 2008.06.20 Dan
;;;             : * Removed the -fct from slot-specs-to-chunk-spec-list since
;;;             :   the macro was removed from the chunk-spec code.
;;; 2008.07.09 Dan 
;;;             : * Changed the p function so that invalid lhs conditions don't
;;;             :   lead to an error during the sort.
;;; 2008.07.22 Dan
;;;             : * Updated whynot so that it notes if a production was below
;;;             :   the utility threshold the last time it was in the conflict
;;;             :   set.
;;; 2008.08.01 Dan
;;;             : * Updated pmatches to work the same as conflict-resolution
;;;             :   since they use the same 'sorter'.
;;; 2008.08.05 Dan
;;;             : * Added the production-failure-reason function needed by the
;;;             :   new production history tool here to keep the code which
;;;             :   accesses the native production structure in the 'system'
;;;             :   code.
;;; 2008.10.23 Dan
;;;             : * Fixed a bug introduced with the "fix" on 2008/07/09 that
;;;             :   could cause an invalid LHS condition to allow the production
;;;             :   to still be created with a null LHS.
;;; 2008.11.03 Dan
;;;             : * Updated the parsing of productions so that the LHS code uses
;;;             :   the new cr-buffer-read option in the conflict resolution
;;;             :   code.
;;; 2008.11.25 Dan
;;;             : * Changed things to use add-production and remove-production
;;;             :   to better abstract away from the internals of the module.
;;;             : * Changed calls to get-production to use get-production-internal
;;;             :   to avoid the unnecessary hash-table look-up when possible.
;;;             : * Abstracting away from procedural-productions with productions-
;;;             :   list so less code is dependent on the procedural structure.
;;;             : * Clear-productions now iteratively removes the productions
;;;             :   instead of just erasing the list.
;;; 2008.12.08 Dan [1.1]
;;;             : * Significant overhaul of the internal representation of 
;;;             :   productions.  No longer create lambdas for the conditions.  
;;;             :   Instead, create more specific single condition tests.
;;;             : * Whynot information can now be more specific - lists which
;;;             :   slot in a buffer caused the mismatch.
;;;             : * Pmatches (and thus whynot too) no longer computes the
;;;             :   utilities and thus it doesn't sort the conflict-set based
;;;             :   on those values.
;;; 2009.05.05 Dan
;;;             : * Changed the inequality tests so that they all use the "right"
;;;             :   test instead of only testing > and negating and then add the
;;;             :   opposite test to the implicit ones for the tree.
;;; 2009.05.22 Dan
;;;             : * Fixed a bug in production parsing which would allow something
;;;             :   like this to "work" (p name =goal> free ==> ...).
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
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The user functions mostly from ACT-R 5

(defun all-productions ()
  (let ((prod (get-module procedural)))
    (when prod
      (mapcar #'production-name (productions-list prod)))))

(defmacro pp (&rest productions)
  `(pp-fct ',productions))

(defun pp-fct (productions)
  (let ((prod (get-module procedural)))
    (if prod
        (let ((res nil)
              (p (if (null productions) 
                     (mapcar #'production-name (productions-list prod))
                   productions)))
          (dolist (p-name p)
            (let ((production (get-production-internal p-name prod)))
              (if production
                  (progn
                    (print-production production)
                    (push p-name res))
                (print-warning "No production named ~S is defined" p-name))))
          (reverse res))
      (print-warning "No procedural module found"))))

(defun clear-productions ()
  (let ((prod (get-module procedural)))
    (if prod
        (progn
          (print-warning "Clearing the productions is not recommended")
          (dolist (p (productions-list prod))
            (remove-production p prod)))
      (print-warning "No procedural module was found."))))


(defmacro pbreak (&rest productions)
  `(pbreak-fct ',productions))

(defun pbreak-fct (productions)
  (if (current-model)
      (progn
        (dolist (production productions)
          (aif (get-production production)
               (setf (production-break it) t)
               (print-warning "~s is not the name of a production" production)))
        (let ((res nil))
          (dolist (production (all-productions) res)
            (when (production-break (get-production production))
              (push production res)))))
    (print-warning "There is no current model - pbreak cannot be used.")))


(defmacro punbreak (&rest productions)
  `(punbreak-fct ',productions))

(defun punbreak-fct (productions)
  (if (current-model)
      (progn
        (dolist (production (if (null productions)
                                (all-productions)
                              productions))
          (aif (get-production production)
               (setf (production-break it) nil)
               (print-warning "~s is not the name of a production" production)))
        (let ((res nil))
          (dolist (production (all-productions) res)
            (when (production-break (get-production production))
              (push production res)))))
    (print-warning "There is no current model - punbreak cannot be used.")))
  



(defmacro pdisable (&rest productions)
  `(pdisable-fct ',productions))

(defun pdisable-fct (productions)
  (if (current-model)
      (progn
        (dolist (production productions)
          (aif (get-production production)
               (setf (production-disabled it) t)
               (print-warning "~s is not the name of a production" production)))
        (let ((res nil))
          (dolist (production (all-productions) res)
            (when (production-disabled (get-production production))
              (push production res)))))
    (print-warning "There is no current model - pdisable cannot be used.")))

(defmacro penable (&rest productions)
  `(penable-fct ',productions))

(defun penable-fct (productions)
  (if (current-model)
      (progn
        (dolist (production (if (null productions)
                                (all-productions)
                              productions))
          (aif (get-production production)
               (setf (production-disabled it) nil)
               (print-warning "~s is not the name of a production" production)))
        (let ((res nil))
          (dolist (production (all-productions) res)
            (when (production-disabled (get-production production))
              (push production res)))))
    (print-warning "There is no current model - penable cannot be used.")))

(defmacro whynot (&rest productions)
  `(whynot-fct ',productions))


(defun whynot-fct (productions)
  (if (current-model) 
      (let* ((procedural (get-module procedural))
             (conflict-set (no-output (pmatches-internal procedural))))
        
        (dolist (production-name (if (null productions)
                                     (all-productions)
                                   productions))
          
          (let ((production (get-production production-name)))
            (if (null production)
                (command-output "~%~s does not name a production." production-name)
              (if (production-disabled production)
                  (command-output "~%Production ~s is disabled." production-name)
                (if (member production-name conflict-set)
                    (progn
                      (command-output "~%Production ~s matches:" production-name)
                      (print-instantiation production)
                      (let ((ut (car (no-output (sgp :ut)))))
                        (when (and (numberp ut)
                                   (numberp (production-utility production-name))
                                   (< (production-utility production-name) ut))
                          (command-output "Utility was below the threshold the last time it was in the conflict set."))))
                  (progn
                    (command-output "~%Production ~s does NOT match." production-name)
                    
                    (print-production production)
                    
                    (command-output "It fails because: ")
                    (command-output (failure-reason-string (production-failure-condition production) procedural production))))))))
        conflict-set)
    (print-warning "Whynot called with no current model.")))


(defun production-failure-reason (p-name)
  (let ((procedural (get-module procedural))
        (production (get-production p-name)))
    (if (and production (production-failure-condition production))
        (failure-reason-string (production-failure-condition production) procedural production)
      "")))

(defun pmatches ()
  (let ((procedural (get-module procedural)))
    (if procedural
        (pmatches-internal procedural)
      (print-warning "No procedural module found"))))
    
(defun pmatches-internal (procedural)
  
  (setf (procedural-buffer-lookup procedural) (make-array (list (procedural-buffer-lookup-size procedural)) :initial-element :untested))
  (setf (procedural-slot-lookup procedural) (make-array (list (procedural-buffer-lookup-size procedural) (largest-chunk-type-size)) :initial-element :untested))
  
  (let* ((conflict-set nil))
    
    (dolist (production (productions-list procedural))
      
      (setf (production-bindings production) nil)
      (setf (production-failure-condition production) nil)
      
      (unless (production-disabled production)
                
        (let ((constant-tests (if (null (production-constants production)) (cons t nil)
                                        (do* ((tests (production-constants production) (cdr tests))
                                              (result (test-constant-condition procedural (car tests))
                                                      
                                                      (test-constant-condition procedural (car tests))))
                                             
                                             ((or (null result) (null (cdr tests))) (cons result (car tests)))))))
          
          (if (null (car constant-tests))
              (setf (production-failure-condition production) (cdr constant-tests))
            
            (let ((user-bindings (if (null (production-binds production)) (cons t nil)
                                   (do* ((tests (production-binds production) (cdr tests))
                                         (result (test-and-perfrom-bindings procedural (car tests) production)
                                                 
                                                 (test-and-perfrom-bindings procedural (car tests) production)))
                                        
                                        ((or (null result) (null (cdr tests))) (cons result (car tests)))))))
                   
                   
                   
                   
              
              (if (null (car user-bindings))
                  (setf (production-failure-condition production) (cdr user-bindings))
                    
                
                (let ((others-pass (if (null (production-others production)) (cons t nil)
                                     
                                     (do* ((conditions (production-others production) (cdr conditions))
                                           (result (test-other-condition procedural (car conditions) production)
                                                   (test-other-condition procedural (car conditions) production)))
                                          ((or (null result) (null (cdr conditions))) (cons result (car conditions)))))))
              
                  (if (null (car others-pass))
                      (setf (production-failure-condition production) (cdr others-pass))
                    (push-last production conflict-set)))))))))
          
    (dolist (production conflict-set)
      (print-instantiation production))
          
    (mapcar #'production-name conflict-set)))

                                         
                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This section is production parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-p (&rest definition)
  "Production definition."
  `(p-fct ',definition))

(defun define-p-fct (definition)
  (p-fct definition))

(defmacro p (&rest definition)
  "Production definition."
  `(p-fct ',definition))

(defun p-fct (definition)
  (let ((prod (get-module procedural)))  
    (if (procedural-p prod)  
        (let ((production (make-production :text (copy-tree definition))))
          
          (unless (symbolp (car definition))
            (print-warning "Production name must be a symbol.")
            (print-warning "No production defined for ~S." definition)
            (return-from p-fct nil))
          
          (setf (production-name production) (pop definition))
          
          (when (stringp (car definition))
            (setf (production-documentation production) (pop definition)))
          
          (aif (position '==> definition)
               (let* ((pre-lhs (parse-conditions prod (subseq definition 0 it)))
                      (lhs (if (eq pre-lhs :error) :error (sort-for-binding pre-lhs)))
                      (rhs (unless (eq lhs :error)
                             (parse-actions prod (subseq definition (1+ it)) lhs))))
                 (when (or (eq lhs :error)
                           (eq rhs :error))
                   (print-warning "No production defined for ~s." (production-text production))
                   (return-from p-fct nil))
                 
                 
                 ;; put back for compilation to use
                 
                 (setf (production-lhs production) lhs)
                 (setf (production-rhs production) rhs)
                 
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
                       (lhs-variables (mapcan 'find-variables (mapcar 'second lhs))))
                   
                   (setf (production-variables production)
                     (remove-duplicates 
                      (append variables
                              (mapcar #'(lambda (x)
                                          (intern (concatenate 'string "=" (symbol-name x))))
                                (production-lhs-buffers production)))))
                   
                   ;(pprint (production-variables production))
                    
                   (setf (production-bindings production) variables)
                   (let ((constants nil)
                         (binds nil)
                         (others nil)
                         (vars nil)
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
                                  (var-spec-list (fourth c))
                                  (others-spec-list (copy-tree (fifth c)))
                                  (ct (chunk-spec-chunk-type constant)))
                              
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
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'chunk-slot-equal :slot (second spec) :si (get-slot-index ct (second spec)) :result (if (third spec) nil t)) implicit)
                                   ;; implicit test for numbers to allow better filtering when any of the relative tests used
                                   (push (make-cr-condition :type 'test-slot :buffer buffer :bi bi :value nil :test 'number-test :slot (second spec) :si (get-slot-index ct (second spec)) :result (if (numberp (third spec)) t nil)) implicit))
                                  
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
                                    
                                    (let ((binding-slot (second v)))
                                      (push-last (make-cr-condition :type 'bind-slot :buffer buffer :bi bi :slot binding-slot :si (get-slot-index ct binding-slot) :value var) vars)
                                      
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
                              
                              
                              (dolist (spec others-spec-list) ;; slots that contained variables
                                
                                (case (car spec)
                                  
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
                                   )))
                              
                                
                          
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
                              (push-last (make-cr-condition :type 'eval :value (car (second c))) others))
                              ((bind safe-bind)
                               
                               (unless (find (car (second c)) (production-bindings production))
                                 (print-warning "Cannot have two explicit bindings for variable ~S" (car (second c)))
                                 (print-warning "No production defined for ~s." (production-text production))
                                 
                                 (return-from p-fct nil))
                               
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
                                     
                                     (return-from p-fct nil)))
                                 
                                   (dolist (x bind-vars)
                                    (setf (production-bindings production)
                                      (remove x (production-bindings production))))
                                 
                                 (push (cons
                                        (cons bind-vars (find-variables (second (second c))))
                                        (make-cr-condition :type 'mv-bind :value bind-vars :result (second (second c))))
                                       binds)))
                             )))))
                     
                     ;;; Make sure everything necessary is bound
                     
                     (awhen (some (lambda (x) (find x (production-bindings production))) lhs-variables)
                        (print-warning "No production defined for ~s because ~s is not bound on the LHS." (production-text production) it)
                        (return-from p-fct nil))
                     
                     ;;; Check the explicit bindings for circularities
                     
                     (when (circular-references binds)
                       (print-warning "No production defined for ~s because there are circular references in the explicit LHS !bind!'s." (production-text production))
                       (return-from p-fct nil))
                     
                     (setf binds (mapcar #'cdr (sort binds #'bind-order :key #'car)))
                     
                     ;;; save the conditions                     
                      
                     (setf (production-constants production) constants)
                     (setf (production-binds production) (append vars binds))
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
                            
                            (return-from p-fct nil))
                   
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
                                        (schedule-mod-buffer-chunk (cdar a)
                                                                   (replace-variables (second a) (production-bindings production))
                                                                   0
                                                                   :module 'procedural
                                                                   :priority 100
                                                                   :output (procedural-rhst prod)))
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
                                                                 :output nil))
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
                        
                        
                        (mv-bind
                         (let ((bind-vars (car (second a))))
                           
                           
                           (dolist (x bind-vars)
                             (unless (find x (production-bindings production))
                               (print-warning "Cannot have two explicit bindings for variable ~S" x)
                               (print-warning "No production defined for ~s." (production-text production))
                               
                               (return-from p-fct nil)))
                           
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
                                              (print-production-output (second a)
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
                          (dolist (slot (chunk-spec-slot-spec (third x)))
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
                                     (return-from p-fct nil)))
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
                                     (return-from p-fct nil)))))))
                 
                 ;; warn modules which request it
                 
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


(defun number-test (val ignore)
  (declare (ignore ignore))
  (numberp val))

(defun sort-for-binding (condition-list)
  (stable-sort (copy-tree condition-list) (lambda (x y) (and (eq x #\!) (not (eq y #\!)))) :key #'caar))

(defun circular-references (bindings)
  (let ((checks (mapcar #'car bindings)))
    
    ;;; Actually modify the dependencies info so that bind-order can use it...

    (dolist (x checks)
      (dolist (y (cdr x))
        (awhen (find y checks :test (lambda (i j) (if (listp (car j))
                                                      (find i (car j))
                                                    (eq i j))))
                                                             
               (setf (cdr x) (append (cdr x) (cdr it))))))
    
    (some (lambda (x) 
            (if (listp (car x))
                (some (lambda (z) (find z (cdr x))) (car x))
              (find (car x) (cdr x)))) checks)))
    
(defun bind-order (x y)
  (if (listp (car x))
      (some (lambda (z) (find z (cdr y))) (car x))
    (find (car x) (cdr y))))


(defun delete-production (prod-name)
  (let ((procedural (get-module procedural)))
    (if procedural
        (remove-production (get-production-internal prod-name procedural) procedural)
      (print-warning "No procedural module found.  Cannot delete production ~S." prod-name))))


(defun parse-conditions (procedural definition)
  (aif (gethash definition (procedural-condition-parse-table procedural))
       it
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
            
           ((null seg) (setf (gethash definition (procedural-condition-parse-table procedural)) conditions))
               
        (case (car cmd)
          (#\=
           (if (< (length (cdr seg)) 2)
               (progn
                 (print-warning "Missing chunk-type test in condition for ~s buffer." (cdr cmd))
                 (return-from parse-conditions :error))
             (aif (define-chunk-spec-fct (cdr seg))
                  (progn
                    (when (some #'keywordp (chunk-spec-slots it))
                      (print-warning "Request parameters not allowed in buffer tests: ~s" seg)
                      (return-from parse-conditions :error))
                    
                    (let* ((slot-specs (chunk-spec-slot-spec it))
                           (constants)
                           (variables)
                           (others))
                      
                      (dolist (spec slot-specs)
                        
                        (cond ((not (chunk-spec-variable-p (third spec)))
                               (push-last spec constants))
                              ((eq '= (first spec))
                               (push-last spec variables)
                               (push-last spec others))
                              (t
                               (push-last spec others))))
                      
                      (push-last (list cmd (cdr seg) 
                                       (define-chunk-spec-fct (slot-specs-to-chunk-spec-list (chunk-spec-chunk-type it) constants))
                                       variables
                                       slot-specs
                                       ;; Add the new item to the end...
                                       (when others (slot-specs-to-chunk-spec-list (chunk-spec-chunk-type it) others)))
                                 conditions)))
                  (progn
                    (print-warning "Invalid syntax in ~s condition." (car seg))
                    (return-from parse-conditions :error)))))
          (#\?
           (let ((queries (process-query-specs (cdr cmd) (cdr seg))))
             
             (cond ((eq queries :error)
                    (print-warning "Invalid buffer query ~S." seg)
                    (return-from parse-conditions :error))
                   
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
                  (return-from parse-conditions :error))))
             ((mv-bind)
              (if (and (= (length seg) 3)
                       (listp (second seg))
                       (every 'chunk-spec-variable-p (second seg)))
                  (push-last (list cmd (cdr seg)) conditions)
                (progn
                  (print-warning "Invalid mv-bind command: ~s" seg)
                  (return-from parse-conditions :error))))
             ((eval safe-eval)
              (if (= (length seg) 2)
                  (push-last (list cmd (cdr seg)) conditions)
                (progn
                  (print-warning "Invalid eval command: ~s" seg)
                  (return-from parse-conditions :error))))
             (t
              (print-warning "Invalid command: ~s" seg)
              (return-from parse-conditions :error))))))))))


(defun process-query-specs (buffer specs)
  (let ((slots nil))
    (loop 
      (when (null specs) (return slots))
      (let ((spec nil))
        (if (member (car specs) '(= -)) ;; for now only allow = and - queries
            (push (pop specs) spec)
          (push '= spec))
        (when (null specs) (return :error))
        (unless (valid-buffer-query buffer (car specs)) 
          (return :error))
        (push-last (pop specs) spec)
        (when (null specs) (return :error))
        (push-last (pop specs) spec)
        (push spec slots)))))

(defun parse-actions (procedural definition conditions)
  (aif (gethash (cons definition conditions) (procedural-action-parse-table procedural))
       it
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
           
           ((null seg) (setf (gethash (cons definition conditions) (procedural-action-parse-table procedural)) actions))
        (when (null cmd)
          (print-warning "Invalid command on RHS: ~S" (car seg))
          (return-from parse-actions :error))
        
        (case (car cmd)
          (#\-
           (if (= (length seg) 1)
               (push-last (list cmd) actions)
             (progn
               (print-warning "Invalid - buffer command: ~s" seg)
               (return-from parse-actions :error))))
          
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
                        (if (valid-chunk-mod-spec chunk-type (cdr seg))
                            (push-last (list cmd (cdr seg)) actions)
                          (progn
                            (print-warning "Invalid buffer modification ~s." seg)
                            (return-from parse-actions :error)))
                      (progn
                        (print-warning "Cannot modify buffer ~s if not matched on LHS." (cdr cmd))
                        (return-from parse-actions :error)))))))
                    
           (#\+
           (cond ((= (length seg) 1)
                  (print-warning "Buffer request ~s requires some parameters." (car seg))
                  (return-from parse-actions :error))
                 ((= (length seg) 2)
                  (push-last (list cmd (cdr seg)) actions))
                 (t
                  (if (eq (second seg) 'isa)
                      ;; This is a full request
                      (progn
                        (aif (define-chunk-spec-fct (cdr seg))
                             (progn
                               (unless (every #'(lambda (x)
                                                (valid-buffer-request-slot 
                                                 (cdr cmd) 
                                                 (chunk-spec-chunk-type it) x))
                                              (chunk-spec-slots it))
                                 
                                 (print-warning  "Invalid slot value in request: ~s" seg)
                                 (return-from parse-actions :error))
                  
                               (push-last (list cmd (cdr seg)) actions))
                             (progn
                               (print-warning "Invalid syntax in action ~s." seg)
                               (return-from parse-actions :error))))
                    ;; otherwise it's assumed to be a modification request
                    (progn
                      (let* ((lhs-binding (find (cons #\= (cdr cmd)) conditions
                                                :test #'equal :key #'car))
                             (chunk-type (if lhs-binding
                                             (chunk-spec-chunk-type (third lhs-binding))
                                           nil)))
                        (if chunk-type
                            (if (valid-chunk-mod-spec chunk-type (cdr seg))
                                (push-last (list cmd (cdr seg)) actions)
                              (progn
                                (print-warning "Invalid buffer modification ~s." seg)
                                (return-from parse-actions :error)))
                          (progn
                            (print-warning "Cannot modify buffer ~s if not matched on LHS." 
                             (cdr cmd))
                            (return-from parse-actions :error)))))))))
          
          (t
           (case (cdr cmd)
             ((bind safe-bind)
              (if (and (= (length seg) 3)
                       (chunk-spec-variable-p (second seg)))
                  (push-last (list cmd (cdr seg)) actions)
                (progn
                  (print-warning "Invalid bind command: ~s" seg)
                  (return-from parse-actions :error))))
             ((mv-bind)
              (if (and (= (length seg) 3)
                       (listp (second seg))
                       (every 'chunk-spec-variable-p (second seg)))
                  (push-last (list cmd (cdr seg)) actions)
                (progn
                  (print-warning "Invalid mv-bind command: ~s" seg)
                  (return-from parse-actions :error))))
             ((eval safe-eval output)
              (if (= (length seg) 2)
                  (push-last (list cmd (cdr seg)) actions)
                (progn
                  (print-warning "Invalid ~s command: ~s" (cdr cmd) seg)
                  (return-from parse-actions :error))))
             (stop
              (if (= (length seg) 1)
                  (push-last (list cmd) actions)
                (progn
                  (print-warning "Invalid stop command: ~s" seg)
                  (return-from parse-actions :error))))
             (t
              (print-warning "Invalid command: ~s" seg)
              (return-from parse-actions :error))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support functions
;;; Some of these may belong elsewhere and possibly in the framework API
;;;


(defun print-production-output (original data)
  (let ((vals (car data)))
        (cond ((atom vals)
         (model-output "~s" vals))
        ((stringp (caar original))
         (model-output "~?" (car vals) (cdr vals)))
        (t
         (model-output "~{~S ~}" vals)))))



(defun bind-variable (var value production)
  (aif (assoc var (production-bindings production))
       (setf (cdr it) value)
       (progn
         (push (cons var value) (production-bindings production))
         value))) 


(defun find-variables (arg)
  (cond ((listp arg) (remove-duplicates (mapcan 'find-variables arg)))
        ((chunk-spec-variable-p arg) (list arg))
        (t nil)))

(defun replace-variables (arg bindings)
  (cond ((and (consp arg) (eq (last arg) arg))  ;; detect that it's something like (a . b)
         (cons (replace-variables (car arg) bindings)
               (replace-variables (cdr arg) bindings)))
         
         ((listp arg) 
         (mapcar #'(lambda (x)
                     (replace-variables x bindings))
           arg))
        ((chunk-spec-variable-p arg) 
         (aif (assoc arg bindings)
              (cdr it)
              arg))
        (t arg)))



(defun replace-variables-for-eval (arg bindings)
  (cond ((listp arg) 
               
         (cond ((listp (car arg))
                ;; If the head is a list just parse the whole thing
                (mapcar #'(lambda (x)
                            (replace-variables-for-eval x bindings))
                  arg))
               ((eq (car arg) 'quote)
               ;; If it's already quoted don't requote
                (mapcar #'(lambda (x)
                            (replace-variables x bindings))
                  arg))
               (t
                (cons (replace-variables (car arg) bindings)
                      (mapcar #'(lambda (x)
                                  (replace-variables-for-eval x bindings))
                        (cdr arg))))))
               
        ((chunk-spec-variable-p arg) 
         (aif (assoc arg bindings)
              (if (or (chunk-spec-variable-p (cdr it))
                      (stringp (cdr it)))
                  (cdr it)
                (list 'quote (cdr it)))
              arg))
        (t arg)))



(defun segment-production (definition)
  (when definition
    (do ((res nil (push-last where res))
         (where (position-if 'parse-command definition )
                (position-if 'parse-command definition :start (1+ where))))
        ((null where)
         (if (and res (zerop (car res)))
             (mapcar #'(lambda (start end)
                         (subseq definition start end))
               res (push-last nil (cdr res)))
           :error)))))

(defun parse-command (cmd &key (operators '(#\? #\= #\- #\+))
                          (commands '(eval safe-eval bind safe-bind mv-bind stop output)))
  (when (symbolp cmd)
    (let* ((name (symbol-name cmd))
           (len (length name))
           (first-char (aref name 0))
           (last-char (aref name (1- len))))
      (when (> len 2)
        (let ((cmd-name (intern (subseq name 1 (1- len)))))
          (cond ((and (eql first-char #\!) (eql last-char #\!)
                      (find cmd-name commands))
                 (cons #\! cmd-name))
                ((and (eql last-char #\>)
                      (find cmd-name (buffers))
                      (find first-char operators))
                 
                 (cons first-char cmd-name))
                
                ;; Don't worry about warnings here - just parse it
                (t nil)))))))
  
(defun query-list-to-conses (queries)
  (do* ((s (copy-tree queries))
        (slot (pop s) (pop s))
        (value (pop s) (pop s))
        (tests (list (cons slot value))
               (push-last (cons slot value) tests)))
       ((null s) tests)))


(defun valid-chunk-mod-spec (chunk-type modifications-list)
  (if (oddp (length modifications-list))
      (print-warning "Odd length modifications list.")
    (do ((slots nil (cons (car s) slots))
         (s modifications-list (cddr s)))
        ((null s) 
         (and (every #'(lambda (slot)
                         (valid-slot-name slot (get-chunk-type chunk-type)))
                     slots)
              (= (length slots) (length (remove-duplicates slots))))))))
    
(defun valid-buffer-query (buffer-name slot)
  (let ((buf (buffer-instance buffer-name)))
    (find slot (act-r-buffer-queries buf)))) 

(defun valid-buffer-request-param (buffer-name slot)
  (let ((buf (buffer-instance buffer-name)))
    (find slot (act-r-buffer-requests buf))))

(defun valid-buffer-request-slot (buffer-name chunk-type slot)
  (if (keywordp slot)
      (valid-buffer-request-param buffer-name slot)
    (valid-slot-name slot (get-chunk-type chunk-type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A trace filter to restrict things to production firing only.

(defun production-firing-only (event)
  "Filter to show only production firing in the trace"
  (eq (evt-action event) 'production-fired))

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
