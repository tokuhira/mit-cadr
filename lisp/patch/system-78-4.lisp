;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.4
;;; Reason: More 78.3, SAVE-ALL-FILES, GRIND-CHECK-DO
;;; Written 12/09/81 10:04:04 by DLA,
;;; while running on Lisp Machine Thirteen from band 1
;;; with Experimental System 78.3, Experimental ZMail 38.0, microcode 836.



; From file MAKSYS > LISPM2; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(DEFUN MAKE-SYSTEM (SYSTEM &REST KEYWORDS)
  ;; First check whether there is a new system declaration that can be loaded
  (MAYBE-RELOAD-SYSTEM-DECLARATION SYSTEM KEYWORDS)
  (PROGW *MAKE-SYSTEM-SPECIAL-VARIABLES*
    (SETQ *SYSTEM-BEING-MADE* (FIND-SYSTEM-NAMED SYSTEM))
    ;; Do all the keywords
    (DOLIST (KEYWORD KEYWORDS)
      (LET ((FUNCTION (GET KEYWORD 'MAKE-SYSTEM-KEYWORD)))
	(OR FUNCTION
	    (FERROR NIL "~S is not a recognized option" KEYWORD))
	(FUNCALL FUNCTION)))
    ;; Put all compiler messages together for this run
    (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
      ;; Process forms with compiler context
      (DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-BEFORE*)
	(EVAL FORM))
      ;; Do the work of the transformations
      (PERFORM-TRANSFORMATIONS (COLLECT-TOP-LEVEL-TRANSFORMATIONS *SYSTEM-BEING-MADE*))
      ;; Finally process any forms queued by the keywords with compiler context
      (DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-AFTER*)
	(EVAL FORM)))
    ;; Now forms outside of compiler context
    (DOLIST (FORM *MAKE-SYSTEM-FORMS-TO-BE-EVALED-FINALLY*)
      (EVAL FORM)))
  T)

)

; From file COMC > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFCOM COM-COMPILE-BUFFER-CHANGED-FUNCTIONS "Compile any sections which have been edited"
	()
  (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
    (COMPILE-BUFFER-CHANGED-FUNCTIONS *INTERVAL* *NUMERIC-ARG-P*))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

)

; From file COMC > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFCOM COM-COMPILE-CHANGED-FUNCTIONS "Compile any sections which have been edited" ()
  (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
    (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
      (AND (EQ (IF (EQ BUFFER *INTERVAL*) *MAJOR-MODE*
		   (BUFFER-SAVED-MAJOR-MODE BUFFER))
	       'LISP-MODE)
	   (COMPILE-BUFFER-CHANGED-FUNCTIONS BUFFER *NUMERIC-ARG-P*))))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

)

; From file ZMACS > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFUN SAVE-BUFFER (BUFFER &AUX FILE-ID PATHNAME)
  (SETQ FILE-ID (BUFFER-FILE-ID BUFFER)  
	PATHNAME (BUFFER-PATHNAME BUFFER))
  (COND ((NULL FILE-ID)
	 (FS:SET-DEFAULT-PATHNAME (FUNCALL (DEFAULT-PATHNAME) ':NEW-NAME (BUFFER-NAME BUFFER))
				  *PATHNAME-DEFAULTS*)
	 (SETQ PATHNAME (IF *WINDOW* (READ-DEFAULTED-PATHNAME "Save File:" (PATHNAME-DEFAULTS)
							      NIL NIL ':WRITE)
			  (FORMAT QUERY-IO "~&Save file to (Default ~A): "
				  (DEFAULT-PATHNAME *PATHNAME-DEFAULTS*))
			  (MAKE-DEFAULTED-PATHNAME (READLINE) (PATHNAME-DEFAULTS))))
	 (SET-BUFFER-PATHNAME PATHNAME BUFFER)))
  (AND (OR (SYMBOLP FILE-ID)
	   (EQUAL FILE-ID (WITH-OPEN-FILE (S PATHNAME '(:PROBE :ASCII))
			    (AND (NOT (STRINGP S)) (FUNCALL S ':INFO))))
	   (FQUERY '#,`(:SELECT T
			:BEEP T
		        :TYPE READLINE
		        :CHOICES ,FORMAT:YES-OR-NO-P-CHOICES)
		   "~A has been changed on disk since you last read or wrote it.~@
		    Save it anyway? "
		   PATHNAME))
       (WRITE-FILE-INTERNAL PATHNAME BUFFER))
  T)

)

; From file ZMACS > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

;;; This can be called from top-level to try to save a bombed ZMACS
(DEFUN SAVE-ALL-FILES ()
  (DOLIST (BUFFER *ZMACS-BUFFER-LIST*)
    (AND (LET ((BUFFER-TICK (BUFFER-TICK BUFFER))
	       (NODE-TICK (NODE-TICK BUFFER)))
	   (AND (NUMBERP BUFFER-TICK)
		(NUMBERP NODE-TICK)
		(> NODE-TICK BUFFER-TICK)))
	 (FQUERY NIL "Save file ~A ? " (BUFFER-NAME BUFFER))
	 (LET ((*WINDOW* NIL)
	       (*WINDOW-LIST* NIL)
	       (*INTERVAL* NIL)
	       (*TYPEOUT-WINDOW* STANDARD-OUTPUT)
	       (*TYPEIN-WINDOW* STANDARD-OUTPUT)
	       (*NUMERIC-ARG-P* NIL))
	   (SAVE-BUFFER BUFFER)))))

)

; From file GRIND > LMIO; AI:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(DEFUN GRIND-CHECK-DO (EXP)
  (AND (< (LENGTH EXP)
	  ;; Don't get faked into losing by the old format.
	  (IF (OR (LISTP (CADR EXP)) (NULL (CADR EXP))) 3 4))
       (*THROW 'GRIND-MACRO-FAILED 'NOT-A-FORM)))

)

