;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.5
;;; Reason: 78.4 for CC:SALVAGE-EDITOR
;;; Written 12/09/81 13:33:20 by MMcM,
;;; while running on Lisp Machine One from band 3
;;; with Experimental System 78.4, Experimental ZMail 38.0, microcode 836.



; From file COMA > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFCOM COM-DELETE-FORWARD "Delete one or more characters forward." ()
  (LET ((POINT (POINT)))
    (LET ((BP (FORWARD-CHAR POINT *NUMERIC-ARG*)))
      (COND ((NULL BP) (BARF))
	    ((EQ (BP-LINE POINT) (BP-LINE BP))
	     (MUST-REDISPLAY *WINDOW*
			     DIS-LINE
			     (BP-LINE BP)
			     (MIN (BP-INDEX BP) (BP-INDEX POINT))))
	    (T (MUST-REDISPLAY *WINDOW* DIS-TEXT)))
      (FUNCALL (IF *NUMERIC-ARG-P* #'KILL-INTERVAL #'DELETE-INTERVAL) BP POINT)))
  DIS-NONE)

)

; From file COMA > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFCOM COM-RUBOUT "Delete one or more characters backward." ()
  (LET ((POINT (POINT)))
    (LET ((BP (FORWARD-CHAR POINT (- *NUMERIC-ARG*) T)))
      (COND ((EQ (BP-LINE POINT) (BP-LINE BP))
	     (MUST-REDISPLAY *WINDOW*
			     DIS-LINE
			     (BP-LINE BP)
			     (MIN (BP-INDEX BP) (BP-INDEX POINT))))
	    (T (MUST-REDISPLAY *WINDOW* DIS-TEXT)))
      (FUNCALL (IF *NUMERIC-ARG-P* #'KILL-INTERVAL #'DELETE-INTERVAL) BP POINT)))
  DIS-NONE)

)

; From file INDENT > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFUN DELETE-CHARS-CONVERTING-TABS (POINT COUNT &AUX (BP (COPY-BP POINT)))
  ;; Scan across what we will delete, converting tabs to spaces.
  ;; BP gets set to the other end of the range to be deleted.
  (COND ((> COUNT 0)
	 (DOTIMES (I COUNT)
	   (AND (BP-= BP (INTERVAL-LAST-BP *INTERVAL*))
		(RETURN (BEEP)))
	   ;; When moving forward, whenever we find a blank we must
	   ;; convert all tabs within the blanks that follow.
	   (AND (MEMQ (BP-CH-CHAR BP) *BLANKS*)
		(LET ((BP1 (COPY-BP BP)))
		  (DO ()
		      ((OR (BP-= BP1 (INTERVAL-LAST-BP *INTERVAL*))
			   (NOT (MEMQ (BP-CH-CHAR BP1) *BLANKS*))))
		    (COND ((= (BP-CH-CHAR BP1) #\TAB)
			   (TAB-CONVERT BP1 (FORWARD-CHAR BP1 1)))
			  (T (IBP BP1))))))
	   (IBP BP)))
	(T
	 (DOTIMES (I (- COUNT))
	   (AND (BP-= BP (INTERVAL-FIRST-BP *INTERVAL*))
		(RETURN (BEEP)))
	   (AND (= (LDB %%CH-CHAR (BP-CHAR-BEFORE BP)) #\TAB)
		(TAB-CONVERT (FORWARD-CHAR BP -1) BP))
	   (DBP BP))))
  (COND ((EQ (BP-LINE POINT) (BP-LINE BP))
	 (MUST-REDISPLAY *WINDOW* DIS-LINE (BP-LINE POINT)
			 (MIN (BP-INDEX POINT) (BP-INDEX BP))))
	(T (MUST-REDISPLAY *WINDOW* DIS-TEXT)))
  (FUNCALL (IF *NUMERIC-ARG-P* #'KILL-INTERVAL #'DELETE-INTERVAL) POINT BP)
  DIS-NONE)

)

; From file SALVAG > ZWEI; AI:
#8R CADR:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "CADR")))

;;;-*- Mode:LISP; Package:CADR -*-

;;; Save all files on the object machine
(DEFUN SALVAGE-EDITOR ()
  (PKG-GOTO "CADR")				;Lots of stuff doesn't work otherwise
  (DO ((BUFFER-LIST (CC-MEM-READ (1+ (QF-POINTER (QF-SYMBOL 'ZWEI:*ZMACS-BUFFER-LIST*))))
		    (QF-CDR BUFFER-LIST))
       BUFFER)
      ((CC-Q-NULL BUFFER-LIST))
    (SETQ BUFFER (QF-CAR BUFFER-LIST))
    (AND (LET ((NODE-TICK (QF-AR-1 BUFFER (GET-DEFSTRUCT-INDEX 'ZWEI:NODE-TICK 'AREF)))
	       (BUFFER-TICK (QF-AR-1 BUFFER (GET-DEFSTRUCT-INDEX 'ZWEI:BUFFER-TICK 'AREF))))
	   (AND (= DTP-FIX (LOGLDB %%Q-DATA-TYPE NODE-TICK))
		(= DTP-FIX (LOGLDB %%Q-DATA-TYPE BUFFER-TICK))
		(> (LOGLDB %%Q-POINTER NODE-TICK) (LOGLDB %%Q-POINTER BUFFER-TICK))))
	 (LET ((BUFFER-NAME (WITH-OUTPUT-TO-STRING (CC-OUTPUT-STREAM)
			      (CC-Q-PRINT-STRING
				(QF-AR-1 BUFFER
					 (GET-DEFSTRUCT-INDEX 'ZWEI:BUFFER-NAME 'AREF))))))
	   (AND (FQUERY NIL "Save buffer ~A? " BUFFER-NAME)
		(SALVAGE-INTERVAL BUFFER
				  (IF (NOT (CC-Q-NULL
					     (QF-AR-1 BUFFER
						      (GET-DEFSTRUCT-INDEX
							'ZWEI:BUFFER-FILE-ID 'AREF))))
				      BUFFER-NAME
				    (FORMAT QUERY-IO "~&Write ~A to file: " BUFFER-NAME)
				    (READLINE QUERY-IO))))))))

)

; From file DIRED > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))


(DEFUN DIRED-DELETE-FILE (LINE)
  (LET ((ERROR (DELETEF (DIRED-LINE-PATHNAME LINE) NIL)))
    (OR (STRINGP ERROR) (PUTPROP (LOCF (LINE-PLIST LINE)) T ':DELETED))
    ERROR))

)

; From file DIRED > ZWEI; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFUN DIRED-UNDELETE-FILE (LINE)
  (LET ((ERROR (FS:UNDELETEF (DIRED-LINE-PATHNAME LINE) NIL)))
    (OR (STRINGP ERROR) (PUTPROP (LOCF (LINE-PLIST LINE)) NIL ':DELETED))
    ERROR))

)

