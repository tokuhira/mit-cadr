;;; Primitive data structure manipulation for ZWEI.    -*- Mode:LISP; Package:ZWEI -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFUN CREATE-LINE (ARRAY-TYPE SIZE)
  (MAKE-LINE MAKE-ARRAY (*LINE-AREA* ARRAY-TYPE SIZE)
	     LINE-TICK *TICK* LINE-LENGTH SIZE))

(DEFUN CREATE-BP (LINE INDEX &OPTIONAL STATUS INTERVAL)
  (IF STATUS
      (LET ((BP (MAKE-BP BP-LINE LINE BP-INDEX INDEX BP-STATUS STATUS
			 BP-INTERVAL (OR INTERVAL *INTERVAL*))))
        (PUSH BP (LINE-BP-LIST LINE))
	BP)
      (IF INTERVAL
	  (MAKE-BP BP-LINE LINE BP-INDEX INDEX BP-STATUS NIL
		   BP-INTERVAL INTERVAL)
	  (MAKE-TEMP-BP BP-LINE LINE BP-INDEX INDEX))))

(DEFUN COPY-BP (BP &OPTIONAL STATUS)
  (CREATE-BP (BP-LINE BP) (BP-INDEX BP) STATUS (BP-INTERVAL BP)))

;;; With no args, make empty interval.
;;; With one arg, turn string into interval.
;;; With two args, they are limiting bps.
(DEFUN CREATE-INTERVAL (&OPTIONAL ARG1 ARG2 (WITH-TICK-P T) &AUX INTERVAL)
  (SETQ INTERVAL (IF WITH-TICK-P (MAKE-INTERVAL-WITH-TICK INTERVAL-TICK *TICK*)
                     (MAKE-INTERVAL)))
  (OR ARG2
      (LET ((LINE (CREATE-LINE 'ART-STRING 0)))
        (SETF (INTERVAL-FIRST-BP INTERVAL) (CREATE-BP LINE 0 ':NORMAL INTERVAL))
	(SETF (INTERVAL-LAST-BP INTERVAL) (CREATE-BP LINE 0 ':MOVES INTERVAL))))
  (AND ARG1
       (COND (ARG2
              (SETF (INTERVAL-FIRST-BP INTERVAL) ARG1)
              (SETF (INTERVAL-LAST-BP INTERVAL) ARG2))
             (T
              (INSERT (INTERVAL-FIRST-BP INTERVAL) ARG1))))
  INTERVAL)

;;; There are two forms:  (MOVE-BP <bp> <to-bp>) and (MOVE-BP <bp> <line> <index>)
(DEFUN MOVE-BP (BP LINE &OPTIONAL INDEX &AUX OLINE)
  (SETQ OLINE (BP-LINE BP))
  (COND ((NULL INDEX)
	 (AND (BP-STATUS BP) (BP-STATUS LINE)
	      (SETF (BP-INTERVAL BP) (BP-INTERVAL LINE)))
	 (SETQ INDEX (BP-INDEX LINE) LINE (BP-LINE LINE)))
	;; If we were not passed a BP, check that the INDEX is in range.
	((> INDEX (LINE-LENGTH LINE))
	 (FERROR NIL "The index ~O is greater than the length of the line ~S"
		 INDEX LINE)))
  (COND ;; If it is to the same line, there can be no problem.
    ((EQ OLINE LINE)
     (SETF (BP-INDEX BP) INDEX))
    (T
     (COND ((BP-STATUS BP)
	    ;; It is a permanent bp changing lines.  Fix relocation lists.
	    (SETF (LINE-BP-LIST OLINE) (DELQ BP (LINE-BP-LIST OLINE)))
	    (PUSH BP (LINE-BP-LIST LINE))))
     (SETF (BP-LINE BP) LINE)
     (SETF (BP-INDEX BP) INDEX)))
  BP)

;;; Move a BP backward over one character.
;;; Return the bp, altered.
;;; At the beginning of the interval, return nil and don't alter it.
;;; FIXUP-P means return the unaltered bp instead of nil.
(DEFUN DBP (BP &OPTIONAL FIXUP-P)
  (COND ((BP-= BP (INTERVAL-FIRST-BP *INTERVAL*))
	 (AND FIXUP-P BP))
	((= (BP-INDEX BP) 0)
	 (MOVE-BP BP (LINE-PREVIOUS (BP-LINE BP))
		  (LINE-LENGTH (LINE-PREVIOUS (BP-LINE BP)))))
	(T (MOVE-BP BP (BP-LINE BP) (1- (BP-INDEX BP))))))

;;; Move a BP forward over one character.
;;; Return the bp, altered.
;;; At the end of the interval, return nil and don't alter it.
;;; FIXUP-P means return the unaltered bp instead of nil.
(DEFUN IBP (BP &OPTIONAL FIXUP-P)
  (COND ((BP-= BP (INTERVAL-LAST-BP *INTERVAL*))
	 (AND FIXUP-P BP))
	((= (BP-INDEX BP) (LINE-LENGTH (BP-LINE BP)))
	 (MOVE-BP BP (LINE-NEXT (BP-LINE BP)) 0))
	(T (MOVE-BP BP (BP-LINE BP) (1+ (BP-INDEX BP))))))

;;; Mark a buffer as changed
;;; Call this before changing it as it may err out if the buffer is read-only.
(DEFUN MUNG-BP-INTERVAL (BP)
  (LET ((INTERVAL (OR (BP-INTERVAL BP) *INTERVAL*)))
    (AND (EQ (INTERVAL-TICK INTERVAL) ':READ-ONLY) (BARF "Read-only"))
    (SETF (INTERVAL-TICK INTERVAL) (TICK))))

;;; Call this before changing it as it may err out if the buffer is read-only.
(DEFUN MUNG-BP-LINE-AND-INTERVAL (BP &AUX (LINE (BP-LINE BP)))
  (AND (GET (LOCF (LINE-PLIST LINE)) ':DIAGRAM) (BARF "Diagram line"))
  (LET ((INTERVAL (OR (BP-INTERVAL BP) *INTERVAL*)))
    (AND (EQ (INTERVAL-TICK INTERVAL) ':READ-ONLY) (BARF "Read-only"))
    (SETF (INTERVAL-TICK INTERVAL) (TICK)))
  (SETF (LINE-TICK LINE) *TICK*)
  (SETF (LINE-CONTENTS-PLIST LINE) NIL))

;;; Return T if X is after or at LINE, NIL if X is before LINE.
(DEFUN SEARCH-FOR-LINE (X LINE)
  (DO ((FORWARD LINE (LINE-NEXT FORWARD))
       (BACKWARD (LINE-PREVIOUS LINE) (LINE-PREVIOUS BACKWARD)))
      (NIL)
    (COND ((EQ X FORWARD) (RETURN T))
	  ((EQ X BACKWARD) (RETURN NIL))
	  ((NULL FORWARD) (RETURN NIL))
	  ((NULL BACKWARD) (RETURN T)))))

(DEFUN BEG-LINE-P (BP)
  (OR (= (BP-INDEX BP) 0)
      (BP-= BP (INTERVAL-FIRST-BP *INTERVAL*))))

(DEFUN END-LINE-P (BP)
  (OR (= (BP-INDEX BP) (LINE-LENGTH (BP-LINE BP)))
      (BP-= BP (INTERVAL-LAST-BP *INTERVAL*))))

(DEFUN BEG-OF-LINE (LINE)
  (CREATE-BP LINE (IF (EQ LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
		      (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))
		      0)))

(DEFUN END-OF-LINE (LINE)
  (CREATE-BP LINE (IF (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
		      (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))
		      (LINE-LENGTH LINE))))

(DEFUN KILL-INTERVAL (BP1 &OPTIONAL BP2 IN-ORDER-P (FORWARDP T))
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (KILL-RING-SAVE-INTERVAL BP1 BP2 T FORWARDP)
  (DELETE-INTERVAL BP1 BP2 T))

(DEFUN KILL-RING-SAVE-INTERVAL (BP1 &OPTIONAL BP2 IN-ORDER-P FORWARDP)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (COND ((EQ *LAST-COMMAND-TYPE* 'KILL)
	 (INSERT-INTERVAL
	   (LET ((INT (CAR *KILL-RING*)))
	     (COND ((STRINGP INT)
		    (SETQ INT (CREATE-INTERVAL INT))
		    (SETF (CAR *KILL-RING*) INT)))
	     (IF FORWARDP
		 (INTERVAL-LAST-BP INT)
		 (INTERVAL-FIRST-BP INT)))
	   BP1 BP2 T))
	(T (KILL-RING-PUSH (COPY-INTERVAL BP1 BP2 T)))))

(DEFUN KILL-INTERVAL-ARG (BP1 BP2 ARG)
  (IF (PLUSP ARG)
      (KILL-INTERVAL BP1 BP2 T T)
      (KILL-INTERVAL BP2 BP1 T NIL)))

(DEFUN COUNT-LINES (FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  (COND ((NULL TO-BP)
	 (SETQ TO-BP (INTERVAL-LAST-BP FROM-BP)
	       FROM-BP (INTERVAL-FIRST-BP FROM-BP))))
  (OR IN-ORDER-P (ORDER-BPS FROM-BP TO-BP))
  (DO ((LINE (BP-LINE FROM-BP) (LINE-NEXT LINE))
       (LAST-LINE (BP-LINE TO-BP))
       (I 1 (1+ I)))
      ((EQ LINE LAST-LINE) I)))

(DEFUN COUNT-CHARS (FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (LET ((FIRST-LINE (BP-LINE FROM-BP))
	(FIRST-INDEX (BP-INDEX FROM-BP))
	(LAST-LINE (BP-LINE TO-BP))
	(LAST-INDEX (BP-INDEX TO-BP)))
    (COND ((EQ FIRST-LINE LAST-LINE)
	   (- LAST-INDEX FIRST-INDEX))
	  (T (DO ((LINE (LINE-NEXT FIRST-LINE) (LINE-NEXT LINE))
		  (I 1 (+ 1 I (LINE-LENGTH LINE))))
		 ((EQ LINE LAST-LINE)
		  (+ I (- (LINE-LENGTH FIRST-LINE) FIRST-INDEX) LAST-INDEX)))))))

(DEFUN LINE-N-CHARS (LINE)
  (LET ((FIRST-BP (INTERVAL-FIRST-BP *INTERVAL*))
	(LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))
    (- (IF (EQ LINE (BP-LINE LAST-BP))
	   (BP-INDEX LAST-BP)
	   (LINE-LENGTH LINE))
       (IF (EQ LINE (BP-LINE FIRST-BP))
	   (BP-INDEX FIRST-BP)
	   0))))

(DEFUN SWAP-BPS (BP1 BP2)
  (LET ((LINE (BP-LINE BP1))
	(INDEX (BP-INDEX BP1)))
    (MOVE-BP BP1 BP2)
    (MOVE-BP BP2 LINE INDEX)))

(DEFUN FLUSH-BP (BP)
  (LET ((LINE (BP-LINE BP)))
    (SETF (LINE-BP-LIST LINE) (DELQ BP (LINE-BP-LIST LINE)))))

(DEFUN BP-< (BP1 BP2)
  (LET ((LINE1 (BP-LINE BP1))
	(LINE2 (BP-LINE BP2)))
    (COND ((EQ LINE1 LINE2)
	   (< (BP-INDEX BP1) (BP-INDEX BP2)))
	  (T (NOT (SEARCH-FOR-LINE LINE1 LINE2))))))

(DEFUN BP-= (BP1 BP2)
  (AND (EQ (BP-LINE BP1) (BP-LINE BP2))
       (= (BP-INDEX BP1) (BP-INDEX BP2))))

(DEFUN BP-CHAR (BP)
  (LET ((LINE (BP-LINE BP))
	(INDEX (BP-INDEX BP)))
    (COND ((= INDEX (LINE-LENGTH LINE)) #\CR)
	  (T (AREF LINE INDEX)))))

(DEFUN BP-CHAR-BEFORE (BP)
  (LET ((INDEX (BP-INDEX BP)))
    (COND ((ZEROP INDEX) #\CR)
	  (T (AREF (BP-LINE BP) (1- INDEX))))))

;;; Returns either NIL or the thing it deleted.
(DEFUN DELETE-LAST-ELEMENT (LIST)
  (AND (> (LENGTH LIST) 1)
       (DO ((L LIST (CDR L)))
	   ((NULL (CDDR L))
	    (PROG1 (CADR L) (RPLACD L NIL))))))

(DEFUN POINT-PDL-PUSH (BP WINDOW &OPTIONAL EXPLICIT (NOTIFICATION T) &AUX TEM)
  (SETQ TEM (LIST (COPY-BP BP ':NORMAL) (PLINE-OF-POINT T WINDOW BP)))
  (AND EXPLICIT (SETQ TEM (NCONC TEM (NCONS EXPLICIT))))
  (PUSH TEM (WINDOW-POINT-PDL WINDOW))
  (AND (> (LENGTH (WINDOW-POINT-PDL WINDOW)) *POINT-PDL-MAX*)
       (FLUSH-BP (CAR (DELETE-LAST-ELEMENT (WINDOW-POINT-PDL WINDOW)))))
  ;;Calls TYPEIN-LINE-MORE rather than TYPEIN-LINE on the expectation
  ;;that the caller has already typed something.
  (AND NOTIFICATION (TV:SHEET-EXPOSED-P *TYPEIN-WINDOW*)
		    (TYPEIN-LINE-MORE *AUTO-PUSH-POINT-NOTIFICATION*)))

;Rotate nth (1-origin!) element to the front of the list, rotating the
;part of the list before it.  With a negative arg rotate the same amount
;backwards.  With an arg of 1 rotate the whole list BACKWARDS, i.e. bring
;up the same element as with an arg of 2 but store the old front at the back.
;Zero arg is undefined, do nothing I guess.  Note that 2 and -2 do the same thing.
;Doesn't barf if N is too big.  Alters the list in place.
(DEFUN ROTATE-TOP-OF-LIST (LIST N)
  (AND (= (ABS N) 1) (SETQ N (* N -1 (LENGTH LIST))))
  (COND ((PLUSP N)
	 (SETQ N (MIN (LENGTH LIST) N))
	 (DO ((I 0 (1+ I))
	      (LIST LIST (CDR LIST))
	      (NTH (NTH (1- N) LIST) OLD)
	      (OLD))
	     (( I N))
	   (SETQ OLD (CAR LIST))
	   (SETF (CAR LIST) NTH)))
	((MINUSP N)
	 (SETQ N (MIN (LENGTH LIST) (MINUS N)))
	 (DO ((I 1 (1+ I))
	      (LIST LIST (CDR LIST))
	      (FRONT (CAR LIST)))
	     (( I N) (SETF (CAR LIST) FRONT))
	   (SETF (CAR LIST) (CADR LIST)))))
  LIST)

(DEFUN POINT-PDL-POP (WINDOW)
  (LET ((PDL (WINDOW-POINT-PDL WINDOW)))
    (OR PDL (BARF))
    (LET ((ENTRY (CAR PDL)))
      (SETF (WINDOW-POINT-PDL WINDOW) (NCONC (CDR PDL) (RPLACD PDL NIL)))
      (PROG () (RETURN-LIST ENTRY)))))

(DEFUN POINT-PDL-EXCH (BP WINDOW ARG-P ARG &AUX PDL ENTRY)
  (SETQ PDL (WINDOW-POINT-PDL WINDOW))
  (AND (EQ ARG-P ':CONTROL-U) (SETQ ARG 0))
  (DO ((ARG (ABS ARG))
       (PDL (IF (MINUSP ARG) (REVERSE PDL) PDL) (CDR PDL))
       (ENT))
      ((< ARG 0)
       (SETQ ENTRY (OR ENT (BARF))))
    (COND ((THIRD (CAR PDL))
	   (SETQ ENT (CAR PDL)
		 ARG (1- ARG)))))
  (SETF (WINDOW-POINT-PDL WINDOW)
	(CONS (LIST (COPY-BP BP ':NORMAL) (PLINE-OF-POINT NIL WINDOW BP) T)
	      (NCONC (DELQ ENTRY (DEL #'BP-= BP PDL)) (NCONS ENTRY))))
  (PROG () (RETURN-LIST ENTRY)))

;;; Move POINT to a BP, displayed at PLINE (which may be NIL)
(DEFUN POINT-PDL-MOVE (BP PLINE)
  (LET ((INTERVAL (BP-INTERVAL BP)))
    (OR (EQ INTERVAL *INTERVAL*)
	(IF (ZMACS-BUFFER-P INTERVAL T)
	    (MAKE-BUFFER-CURRENT INTERVAL)
	    (SETQ *INTERVAL* INTERVAL))))
  (MOVE-BP (POINT) BP)
  (AND PLINE (REDISPLAY-POINT-ON-PLINE BP *WINDOW* PLINE)))

(DEFUN POINT-PDL-PURGE (BUFFER)
  (DOLIST (WINDOW *WINDOW-LIST*)
    (SETF (WINDOW-POINT-PDL WINDOW)
	  (DEL #'(LAMBDA (BUF POINT) (EQ BUF (BP-INTERVAL (FIRST POINT)))) BUFFER
	       (WINDOW-POINT-PDL WINDOW)))))

(DEFUN ROTATE-POINT-PDL (WINDOW N &AUX POINT ENTRY LIST)
  (SETQ POINT (WINDOW-POINT WINDOW)
	ENTRY (LIST (COPY-BP POINT ':NORMAL) (PLINE-OF-POINT T WINDOW POINT) T)
	LIST (CONS ENTRY (WINDOW-POINT-PDL WINDOW)))
  (ROTATE-TOP-OF-LIST LIST N)
  (SETQ ENTRY (CAR LIST))
  (POINT-PDL-MOVE (CAR ENTRY) (CADR ENTRY))
  DIS-BPS)

(DEFUN KILL-RING-PUSH (INTERVAL)
  (PUSH INTERVAL *KILL-RING*)
  (AND (> (LENGTH *KILL-RING*) *KILL-RING-MAX*)
       (DELETE-LAST-ELEMENT *KILL-RING*)))

(DEFUN MINI-BUFFER-RING-PUSH (THING)
  (PUSH THING *MINI-BUFFER-RING*)
  (AND (> (LENGTH *MINI-BUFFER-RING*) *KILL-RING-MAX*)
       (DELETE-LAST-ELEMENT *MINI-BUFFER-RING*)))

(DEFUN KILL-RING-POP (ARG)
  (AND (MINUSP ARG) (SETQ ARG (+ ARG (LENGTH *KILL-RING*))))
  (AND (OR (NULL *KILL-RING*) (< ARG 0) ( ARG (LENGTH *KILL-RING*))) (BARF))
  (LET ((CDR (NTHCDR ARG *KILL-RING*)))
    (CAR (SETQ *KILL-RING* (NCONC (PROG1 (CDR CDR) (RPLACD CDR NIL)) *KILL-RING*)))))

;;; Change the font of a character or a string
(DEFUN IN-CURRENT-FONT (X &OPTIONAL (FONT *FONT*))
  (COND ((NUMBERP X)
	 (DPB FONT %%CH-FONT X))
	((ZEROP FONT)		;Little efficiency for strings
	 X)
	(T (LET ((LENGTH (STRING-LENGTH X)))
	     (LET ((S (MAKE-ARRAY NIL 'ART-16B LENGTH)))
	       (DO ((I 0 (1+ I)))
		   (( I LENGTH) S)
		 (ASET (DPB FONT %%CH-FONT (AREF X I)) S I)))))))

(DEFUN LINE-BLANK-P (LINE &AUX LIM)
  (COND ((NUMBERP LINE)
	 (MEMQ (LDB %%CH-CHAR LINE) *BLANKS*))
	((GET (LOCF (LINE-PLIST LINE)) ':DIAGRAM) NIL)
	(T
	 (SETQ LIM (LINE-LENGTH LINE))
	 (DO ((I 0 (1+ I)))
	     (( I LIM) T)
	   (OR (MEMQ (LDB %%CH-CHAR (AREF LINE I)) *BLANKS*)
	       (RETURN NIL))))))

(DEFUN RANGE (X MIN MAX)
  (MAX MIN (MIN MAX X)))

;;; If any of the BPs on the point pdl are the same as point,
;;; then they are useless; flush them.  Except, leave at least
;;; one BP on the pdl.
(DEFUN CLEAN-POINT-PDL (WINDOW)
  (DO ((L (WINDOW-POINT-PDL WINDOW) (CDR L))
       (PT (WINDOW-POINT WINDOW)))
      ((OR (NULL (CDR L))
	   (NOT (BP-= (CAAR L) PT)))
       (SETF (WINDOW-POINT-PDL WINDOW) L))
    (FLUSH-BP (CAAR L))))

(DEFUN STRING-MATCH (PATTERN SUBJECT)
  (LET ((PATTERN-LENGTH (STRING-LENGTH PATTERN)))
    (COND ((AND ( (STRING-LENGTH SUBJECT) PATTERN-LENGTH)
		(STRING-EQUAL PATTERN SUBJECT 0 0 PATTERN-LENGTH PATTERN-LENGTH))
	   PATTERN-LENGTH)
	  (T NIL))))

;;; Return :BLANK, :COMMENT, :ATOM or :NORMAL depending on the first non-blank character.
(DEFUN LINE-TYPE (LINE)
  (IF (GET (LOCF (LINE-PLIST LINE)) ':DIAGRAM) ':DIAGRAM
      (DO ((I (IF (EQ LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
		  (BP-INDEX (INTERVAL-FIRST-BP *INTERVAL*))
		  0)
	      (1+ I))
	   (LIM (IF (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
		    (BP-INDEX (INTERVAL-LAST-BP *INTERVAL*))
		    (LINE-LENGTH LINE))))
	  (( I LIM) ':BLANK)
	(LET ((CH (LDB %%CH-CHAR (AREF LINE I))))
	  (OR (MEMQ CH *BLANKS*)
	      (RETURN (COND ((= CH #/;) ':COMMENT)
			    ((= CH #\FF) ':FORM)
			    ((= (LIST-SYNTAX CH) LIST-ALPHABETIC) ':ATOM)
			    (T ':NORMAL))))))))

;;; Uppercasify the character pointed to by BP.
(DEFUN UPCASE-CHAR (BP)
  (LET ((LINE (BP-LINE BP))
	(INDEX (BP-INDEX BP)))
    (COND ((< INDEX (LINE-LENGTH LINE))
	   (MUNG-BP-LINE-AND-INTERVAL BP)
	   (ASET (CHAR-UPCASE (AREF LINE INDEX)) LINE INDEX)))))

;;; Lowercasify the character pointed to by BP.
(DEFUN DOWNCASE-CHAR (BP)
  (LET ((LINE (BP-LINE BP))
	(INDEX (BP-INDEX BP)))
    (COND ((< INDEX (LINE-LENGTH LINE))
	   (MUNG-BP-LINE-AND-INTERVAL BP)
	   (ASET (CHAR-DOWNCASE (AREF LINE INDEX)) LINE INDEX)))))

;;; Uppercasify all characters in the specified interval.
(DEFUN UPCASE-INTERVAL (BP1 &OPTIONAL BP2 IN-ORDER-P)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (MUNG-BP-INTERVAL BP1)
  (CHARMAP-PER-LINE (BP1 BP2 NIL)
      ((MUNG-LINE (CHARMAP-LINE)))
    (LET ((BEFORE (CHARMAP-CHAR)))
      (LET ((AFTER (CHAR-UPCASE BEFORE)))
	(COND ((NOT (= BEFORE AFTER))
	       (CHARMAP-SET-CHAR AFTER)))))))

;;; Lowercasify all characters in the specified interval.
(DEFUN DOWNCASE-INTERVAL (BP1 &OPTIONAL BP2 IN-ORDER-P)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (MUNG-BP-INTERVAL BP1)
  (CHARMAP-PER-LINE (BP1 BP2 NIL)
      ((MUNG-LINE (CHARMAP-LINE)))
    (LET ((BEFORE (CHARMAP-CHAR)))
      (LET ((AFTER (CHAR-DOWNCASE BEFORE)))
	(COND ((NOT (= BEFORE AFTER))
	       (CHARMAP-SET-CHAR AFTER)))))))

(DEFUN UNDO-SAVE (BP1 &OPTIONAL BP2 IN-ORDER-P NAME)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (IF (NOT (BOUNDP '*UNDO-START-BP*))
      (SETQ *UNDO-START-BP* (COPY-BP BP1 ':NORMAL)
	    *UNDO-END-BP* (COPY-BP BP2 ':MOVES))
      (MOVE-BP *UNDO-START-BP* BP1)
      (MOVE-BP *UNDO-END-BP* BP2))
  (SETQ *UNDO-OLD-INTERVAL* (COPY-INTERVAL BP1 BP2 T))
  (KILL-RING-PUSH *UNDO-OLD-INTERVAL*)
  (SETQ *UNDO-TYPE* NAME))

(DEFUN RELEVANT-FUNCTION-NAME (BP &OPTIONAL STRINGP (FUNCTION-ONLY T) &AUX START-BP)
  (SETQ START-BP (FORWARD-DEFUN BP -1 T))
  (DO ((BP1 BP)
       (FN-START)
       (FN-END)
       (X))
      ((NULL (SETQ BP1 (FORWARD-SEXP BP1 -1 NIL 1 START-BP NIL))))
    (OR (SETQ FN-START (FORWARD-CHAR BP1)) (RETURN NIL))
    (OR (SETQ FN-END (FORWARD-SEXP FN-START)) (RETURN NIL))
    (AND (EQ (BP-LINE FN-START) (BP-LINE FN-END))
	 (SETQ X (CAR (ERRSET (READ-FROM-STRING (BP-LINE FN-START) NIL (BP-INDEX FN-START))
			      NIL)))
	 (SYMBOLP X)
	 (OR (NOT FUNCTION-ONLY)
	     (FBOUNDP X)			;Anything you could hope to meta-. to
	     (STRING-IN-AARRAY-P X *ZMACS-COMPLETION-AARRAY*)
	     (GET X 'SOURCE-FILE-NAME))
	 (RETURN (IF STRINGP (FORMAT NIL "~S" X) X)))))

(DEFUN RELEVANT-METHOD-NAME (BP &OPTIONAL (NSEXP 2) &AUX BP1)
  (AND (SETQ BP (FORWARD-LIST BP -1 NIL 1))
       (SETQ BP (FORWARD-LIST BP 1 NIL 1 T))
       (SETQ BP (FORWARD-SEXP BP NSEXP))
       (SETQ BP (FORWARD-TO-WORD BP))
       (SETQ BP1 (FORWARD-ATOM BP))
       (INTERN (STRING-INTERVAL BP BP1 T) "")))

;;; You might want to change this, if e.g. you are only hacking windows
(DEFVAR *BASE-FLAVOR* 'SI:VANILLA-FLAVOR)

(DEFUN METHOD-ARGLIST (MESSAGE-NAME)
  (MULTIPLE-VALUE-BIND (ARGLIST FUN RETLIST)
      (METHOD-ARGLIST-INTERNAL *BASE-FLAVOR* MESSAGE-NAME NIL NIL NIL)
    (MVRETURN (IF ARGLIST (CDR ARGLIST) 'NOT-FOUND) FUN RETLIST)))

(DEFUN METHOD-ARGLIST-INTERNAL (FLAVOR MESSAGE-NAME ARGLIST FUN RETLIST
				&AUX FLAVOR-METHOD-TABLE MESSAGE-ENTRY)
  (SETQ FLAVOR (GET FLAVOR 'SI:FLAVOR))
  (AND (SETQ FLAVOR-METHOD-TABLE (SI:FLAVOR-METHOD-TABLE FLAVOR))
       (SETQ MESSAGE-ENTRY (ASSQ MESSAGE-NAME FLAVOR-METHOD-TABLE))
       (DOLIST (METHOD (CDDDR MESSAGE-ENTRY))
	 (AND (MEMQ (CAR METHOD) '(NIL :BEFORE :AFTER))
	      (LET ((FUNCTION (CADR METHOD)))
		;; FUN is the first method seen, where we assume most of the
		;; argument list names came from.  We are assuming that all methods
		;; for a given message name are more or less compatible.
		(OR FUN (SETQ FUN (EH:FUNCTION-NAME (FSYMEVAL FUNCTION))))
		(MULTIPLE-VALUE-BIND (THISARG THISRET) (ARGLIST FUNCTION)
		  (OR RETLIST (SETQ RETLIST THISRET))
		  (SETQ ARGLIST (METHOD-ARGLIST-MERGE ARGLIST THISARG)))))))
  (DOLIST (FLAVOR (SI:FLAVOR-DEPENDED-ON-BY FLAVOR))
    (MULTIPLE-VALUE (ARGLIST FUN)
      (METHOD-ARGLIST-INTERNAL FLAVOR MESSAGE-NAME ARGLIST FUN RETLIST)))
  (MVRETURN ARGLIST FUN RETLIST))

(DEFUN METHOD-ARGLIST-MERGE (OLD-ARGLIST NEW-ARGLIST)
  (DO ((OLD OLD-ARGLIST (CDR OLD))
       (NEW NEW-ARGLIST (CDR NEW))
       (OLDOLD NIL OLD))
      ((OR (NULL OLD) (NULL NEW)))
    (DO () ((NOT (MEMQ (CAR OLD) '(&OPTIONAL &SPECIAL &LOCAL))))
      (SETQ OLD (CDR OLD)))
    (DO () ((NOT (MEMQ (CAR NEW) '(&OPTIONAL &SPECIAL &LOCAL))))
      (SETQ NEW (CDR NEW)))
    (COND ((EQ (CAR OLD) '&REST)
	   (OR (EQ (CAR NEW) '&REST)
	       (IF OLDOLD (RPLACD OLDOLD NEW) (SETQ OLD-ARGLIST (COPYLIST NEW))))
	   (RETURN))
	  ((EQ (CAR NEW) '&REST)
	   (AND (SYMBOLP (CADR OLD)) (STRING-EQUAL (CADR OLD) 'IGNORE)
		(NOT (AND (SYMBOLP (CADR NEW)) (STRING-EQUAL (CADR NEW) 'IGNORE)))
		(RPLACA (CDR OLD) (CADR NEW)))
	   (RETURN))
	  ((AND (SYMBOLP (CAR OLD)) (STRING-EQUAL (CAR OLD) 'IGNORE))
	   (OR (AND (SYMBOLP (CAR NEW)) (STRING-EQUAL (CAR NEW) 'IGNORE))
	       (RPLACA OLD (CAR NEW))))))
  (OR OLD-ARGLIST (COPYLIST NEW-ARGLIST)))

;;; Return T if BPs are more then N lines apart.  (Used by MAYBE-PUSH-POINT.)
(DEFUN BPS-FAR-APART (BP1 BP2 N)
  (LET ((LINE1 (BP-LINE BP1))
	(LINE2 (BP-LINE BP2)))
    (NOT (OR (DO ((L LINE1 (LINE-NEXT L))
		  (I 0 (1+ I)))
		 (( I N) NIL)
	       (IF (EQ L LINE2) (RETURN T))
	       (IF (NULL L) (RETURN NIL)))
	     (DO ((L LINE1 (LINE-PREVIOUS L))
		  (I 0 (1+ I)))
		 (( I N) NIL)
	       (IF (EQ L LINE2) (RETURN T))
	       (IF (NULL L) (RETURN NIL)))))))

(DEFUN PARAGRAPH-INTERVAL (BP &OPTIONAL (N 1))
  (LET ((TEMP-BP (DO ((BP BP (FORWARD-LINE BP 1)))
		     ((NULL BP)
		      (INTERVAL-LAST-BP *INTERVAL*))
		   (IF (NOT (LINE-BLANK-P (BP-LINE BP)))
		       (RETURN BP)))))
    (SETQ TEMP-BP (FORWARD-PARAGRAPH TEMP-BP N T))
    (SETQ TEMP-BP
	  (DO ((BP TEMP-BP (FORWARD-LINE BP -1)))
	      ((NULL BP)
	       (INTERVAL-FIRST-BP *INTERVAL*))
	    (IF (NOT (LINE-BLANK-P (BP-LINE BP)))
		(RETURN (OR (FORWARD-LINE BP 1) (INTERVAL-LAST-BP *INTERVAL*))))))
    (LET ((OTHER-BP (FORWARD-PARAGRAPH TEMP-BP (- N) T)))
      (CREATE-INTERVAL OTHER-BP TEMP-BP))))

(DEFUN REST-OF-INTERVAL-STREAM (BP)
  (INTERVAL-STREAM BP (INTERVAL-LAST-BP *INTERVAL*) T))

;;; HACK-FONTS T means return 's for font changes
;;; HACK-FONTS :TYO means return 16 bit characters
(DEFUN INTERVAL-STREAM (FROM-BP &OPTIONAL TO-BP IN-ORDER-P &OPTIONAL HACK-FONTS)
  (LOCAL-DECLARE ((SPECIAL *LINE* *INDEX* *LAST-LINE* *LAST-INDEX* *STOP-INDEX* *UNRCHF*
                           *FONT-FLAG*))
    (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
    (LET ((*INTERVAL* (CREATE-INTERVAL FROM-BP TO-BP T))
          (*LINE* (BP-LINE FROM-BP))
          (*INDEX* (BP-INDEX FROM-BP))
          (*LAST-LINE* (BP-LINE TO-BP))
          (*LAST-INDEX* (BP-INDEX TO-BP))
          (*STOP-INDEX* (IF (EQ (BP-LINE FROM-BP) (BP-LINE TO-BP))
                            (BP-INDEX TO-BP)
                            (LINE-LENGTH (BP-LINE FROM-BP))))
          (*UNRCHF* NIL)
          (*FONT-FLAG* 0))
      (AND (EQ HACK-FONTS ':TYI) (SETQ *FONT-FLAG* NIL HACK-FONTS NIL))
      (CLOSURE '(*INTERVAL* *LINE* *INDEX* *LAST-LINE* *LAST-INDEX* *STOP-INDEX* *UNRCHF*
			    *FONT-FLAG*)
	       (IF HACK-FONTS 'INTERVAL-WITH-FONTS-IO 'INTERVAL-IO)))))

;;; *LINE*, *INDEX* point to the next character to be returned.
;;; *STOP-INDEX* is the place on the current line at which to stop (usually the end).
;;; *LAST-LINE*, *LAST-INDEX* is where the interval ends.
;;; If *INDEX* is NIL, we are at the end-of-file.

(LOCAL-DECLARE ((SPECIAL *LINE* *INDEX* *LAST-LINE* *LAST-INDEX* *STOP-INDEX* *UNRCHF*
			 *FONT-FLAG*))
(DEFSELECT (INTERVAL-IO INTERVAL-IO-DEFAULT-HANDLER)
  (:TYI (&OPTIONAL EOF &AUX CH)
   (COND (*UNRCHF*
	  (PROG1 *UNRCHF* (SETQ *UNRCHF* NIL)))
	 ((NULL *INDEX*)
	  (AND EOF (ERROR EOF)))
	 ((< *INDEX* *STOP-INDEX*)
	  (SETQ CH (AREF *LINE* *INDEX*))
	  (AND *FONT-FLAG* (SETQ CH (LDB %%CH-CHAR CH)))
	  (SETQ *INDEX* (1+ *INDEX*))
	  CH)
	 ((EQ *LINE* *LAST-LINE*)
	  (SETQ *INDEX* NIL)
	  (AND EOF (ERROR EOF)))
	 (T
	  (SETQ *LINE* (LINE-NEXT *LINE*))
	  (SETQ *INDEX* 0)
	  (SETQ *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
				 *LAST-INDEX*
				 (LINE-LENGTH *LINE*)))
	  #\CR)))
  (:LINE-IN (&OPTIONAL SIZE EOF)
   ;; First, if there is an unread character, discard it and back up one.
   (COND (*UNRCHF*
	  (SETQ *UNRCHF* NIL)
	  (IF (ZEROP *INDEX*)
	      (SETQ *LINE* (LINE-PREVIOUS *LINE*) *INDEX* (LINE-LENGTH *LINE*))
	      (SETQ *INDEX* (1- *INDEX*)))))
   (LET ((RET-LINE)
	 (AT-END-P (EQ *LINE* *LAST-LINE*)))
     (COND ((AND (NULL SIZE)
		 (ZEROP *INDEX*)
		 (NOT AT-END-P))
	    ;; Easy case, just return the line and advance the pointer.
	    (SETQ RET-LINE *LINE*)
	    (SETQ *LINE* (LINE-NEXT *LINE*))
	    (SETQ *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
				   *LAST-INDEX*
				   (LINE-LENGTH *LINE*))))
	   ((NULL *INDEX*)
	    ;; End of file.
	    (AND EOF (ERROR EOF)))
	   (T
	    ;; Hard case, make a copy.
	    (SETQ RET-LINE
		  (MAKE-ARRAY NIL (ARRAY-TYPE *LINE*) (- *STOP-INDEX* *INDEX*)
			      NIL (IF (NUMBERP SIZE) SIZE NIL)))
	    (DO ((LF *INDEX* (1+ LF))
		 (RT 0 (1+ RT)))
		(( LF *STOP-INDEX*))
	      (ASET (AREF *LINE* LF) RET-LINE RT))
	    (IF (NUMBERP SIZE)
		(STORE-ARRAY-LEADER (- *STOP-INDEX* *INDEX*) RET-LINE 0))
	    ;; Now advance the pointer.
	    (COND (AT-END-P
		   (SETQ *INDEX* NIL))
		  (T
		   (SETQ *LINE* (LINE-NEXT *LINE*))
		   (SETQ *INDEX* 0)
		   (SETQ *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
					  *LAST-INDEX*
					  (LINE-LENGTH *LINE*)))))))
     (MVRETURN RET-LINE AT-END-P)))
  (:UNTYI (CH)
   (SETQ *UNRCHF* CH))
  (:TYO (CH)
   (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*) CH)))
     (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))
  (:LINE-OUT (LINE)
   (COND ((ZEROP *INDEX*)                  ;Optimize normal case
	  (INSERT-LINE-WITH-LEADER LINE *LINE*))
	 (T
	  (LET ((BP (INSERT
		     (INSERT (CREATE-BP *LINE* *INDEX*) LINE)
		     #\CR)))
	    (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP)))))
   LINE)
  (:STRING-OUT (STRING)
   (LET ((BP (INSERT (CREATE-BP *LINE* *INDEX*) STRING)))
     (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))))
  ((:UNTYO-MARK :READ-BP) ()
   (CREATE-BP *LINE* *INDEX*))
  (:UNTYO (MARK)
   (DELETE-INTERVAL MARK (CREATE-BP *LINE* *INDEX*) T)
   (SETQ *LINE* (BP-LINE MARK) *INDEX* (BP-INDEX MARK)))
  (:SET-BP (BP)
   (SETQ *LINE* (BP-LINE BP) *INDEX* (BP-INDEX BP))
   (SETQ *UNRCHF* NIL)
   (LET ((LAST-BP (INTERVAL-LAST-BP *INTERVAL*)))	;Take account of inserted changes
     (SETQ *LAST-LINE* (BP-LINE LAST-BP)
	   *LAST-INDEX* (BP-INDEX LAST-BP)))
   (SETQ *STOP-INDEX* (IF (EQ *LINE* *LAST-LINE*)
			  *LAST-INDEX*
			  (LINE-LENGTH *LINE*))))
  (:DELETE-TEXT ()
   (DELETE-INTERVAL *INTERVAL*))
  (:FRESH-LINE ()
   (OR (ZEROP *INDEX*) (INTERVAL-IO ':TYO #\CR)))
  (:SET-POINTER (POINTER)
   (OR (ZEROP POINTER) (FERROR NIL "Attempt to set pointer other than to beginning."))
   (LET ((BP (INTERVAL-FIRST-BP *INTERVAL*)))
     (SETQ *LINE* (BP-LINE BP)
	   *INDEX* (BP-INDEX BP))))
  (:READ-CURSORPOS (&OPTIONAL UNITS)
    (OR (EQ UNITS ':CHARACTER) (FERROR NIL "~S unknown cursor-position unit" UNITS))
    (DO ((I 0 (1+ I))
	 (X 0))
	(( I *INDEX*) (RETURN X 0))	;Y position always zero
      (SELECTQ (LDB %%CH-CHAR (AREF *LINE* I))
	(#\BS (SETQ X (MAX (1- X) 0)))
	(#\TAB (SETQ X (* (1+ (// X 8)) 8)))
	(OTHERWISE (SETQ X (1+ X))))))
  (:SET-CURSORPOS (X Y &OPTIONAL (UNITS ':PIXEL))
    Y					;This is a bit fraudulent, for FORMAT ~T.  Ignores Y.
    (OR (EQ UNITS ':CHARACTER) (FERROR NIL "~S unknown cursor-position unit" UNITS))
    ;; Can't use the regular indent stuff since we don't have a window.
    (LET ((FROM (INTERVAL-IO ':READ-CURSORPOS ':CHARACTER))
	  (TO X))
      (DO FROM FROM (1+ FROM) ( FROM TO)
	(INTERVAL-IO ':TYO #\SP))))))

(DEFUN INTERVAL-IO-DEFAULT-HANDLER (OP &OPTIONAL ARG1 &REST REST)
  (STREAM-DEFAULT-HANDLER 'INTERVAL-IO OP ARG1 REST))
 
(DEFUN GRIND-INTO-BP (BP SEXP)
  (SI:GRIND-TOP-LEVEL SEXP 90. (INTERVAL-STREAM BP BP T) T))

;;; Read a number out of a string (starting at FROM, in the given RADIX).
;;; Returns the number, or NIL if no number was seen.
;;; Second value returned is where in the string the number ended
;;; (index of first non-digit).
(DEFUN PARSE-NUMBER (STRING &OPTIONAL (FROM 0) (RADIX 10.))
  (DO ((I FROM (1+ I))
       (CH)
       (NUM 0)
       (FIRSTP T NIL)
       (LIM (STRING-LENGTH STRING)))
      (NIL)
    (AND ( I LIM)
	 (RETURN (AND (NOT FIRSTP) NUM) I))
    (SETQ CH (AREF STRING I))
    (COND ((OR (< CH #/0)
	       (> CH #/9))
	   (RETURN (AND (NOT FIRSTP) NUM) I)))
    (SETQ NUM (+ (* NUM RADIX) (- CH #/0)))))

;;; "Print" a number into an array the fast way
(DEFUN NUMBER-INTO-ARRAY (ARRAY N &OPTIONAL (RADIX BASE) (AT-INDEX 0) (MIN-COLUMNS 0)
				  &AUX QUOT)
  (IF (ZEROP (SETQ QUOT (// N RADIX)))
      (DOTIMES (I (1- MIN-COLUMNS))
	(ASET #\SP ARRAY AT-INDEX)
	(SETQ AT-INDEX (1+ AT-INDEX)))
      (SETQ AT-INDEX (NUMBER-INTO-ARRAY ARRAY QUOT RADIX AT-INDEX (1- MIN-COLUMNS))))
  (ASET (+ #/0 (\ N RADIX)) ARRAY AT-INDEX)
  (1+ AT-INDEX))

;;; Add an array to the end of another
(DEFUN APPEND-TO-ARRAY (TO-ARRAY FROM-ARRAY &OPTIONAL (FROM-START 0) FROM-END
					    &AUX OLD-LENGTH NEW-LENGTH)
  (OR FROM-END (SETQ FROM-END (ARRAY-ACTIVE-LENGTH FROM-ARRAY)))
  (SETQ NEW-LENGTH (+ (SETQ OLD-LENGTH (ARRAY-LEADER TO-ARRAY 0)) (- FROM-END FROM-START)))
  (AND (< (ARRAY-LENGTH TO-ARRAY) NEW-LENGTH) (ADJUST-ARRAY-SIZE TO-ARRAY NEW-LENGTH))
  (COPY-ARRAY-PORTION FROM-ARRAY FROM-START FROM-END TO-ARRAY OLD-LENGTH NEW-LENGTH)
  (STORE-ARRAY-LEADER NEW-LENGTH TO-ARRAY 0))

;;; Is the text immediately following BP the same as the contents of STRING?
;;; If string contains newlines, this doesn't work.
(DEFUN LOOKING-AT (BP STRING)
  (LET ((CP (BP-INDEX BP))
	(SLEN (STRING-LENGTH STRING)))
    (STRING-EQUAL (BP-LINE BP) STRING CP 0 (+ CP SLEN))))

;;; Is the text immediately before BP the same as the contents of STRING?
;;; If string contains newlines, this doesn't work.
(DEFUN LOOKING-AT-BACKWARD (BP STRING)
  (LET ((CP (BP-INDEX BP))
	(SLEN (STRING-LENGTH STRING)))
    (AND ( CP SLEN)
	 (STRING-EQUAL (BP-LINE BP) STRING (- CP SLEN) 0 CP))))

;;; Is this character a delimiter?
(DEFUN DELIMCHAR-P (CHAR)
  (LET ((CH (CHAR-UPCASE (LDB %%CH-CHAR CHAR))))
    (NOT (OR (AND ( CH #/A) ( CH #/Z))
	     (AND ( CH #/0) ( CH #/9))))))

(DEFUN BP-LOOKING-AT-LIST (BP LIST)
  (DO ((LIST LIST (CDR LIST))
       (BP-CH (BP-CH-CHAR BP))
       (CH))
      ((NULL LIST) NIL)
    (AND (IF (NUMBERP (SETQ CH (CAR LIST)))
	     (CHAR-EQUAL BP-CH CH)
	     (LET ((LEN (STRING-LENGTH CH))
		   (INDEX (BP-INDEX BP)))
	       (STRING-EQUAL (BP-LINE BP) CH INDEX 0 (+ INDEX LEN) LEN)))
	 (RETURN CH))))

;Return a list of callers of a function, like WHO-CALLS prints
;The symbol UNBOUND-FUNCTION is treated specially here too.
(LOCAL-DECLARE ((SPECIAL FUNCTION LIST))
(DEFUN LIST-CALLERS (FUNCTION &OPTIONAL (PKG PKG-GLOBAL-PACKAGE) &AUX LIST)
  (FUNCALL (IF (EQ PKG PKG-GLOBAL-PACKAGE) #'MAPATOMS-ALL #'MAPATOMS) #'LIST-CALLERS-AUX PKG)
  LIST)

(DEFUN LIST-CALLERS-AUX (CALLER &AUX TEM DEFN SYM)
  (COND ((FBOUNDP CALLER)
	 (SETQ DEFN (FSYMEVAL CALLER))
	 (AND (LISTP DEFN) (EQ (CAR DEFN) 'MACRO) (SETQ DEFN (CDR DEFN)))
	 (COND ((LISTP DEFN)
		(LIST-CALLERS-1 CALLER FUNCTION DEFN))
	       ((= (%DATA-TYPE DEFN) DTP-FEF-POINTER)
		(DO ((I SI:%FEF-HEADER-LENGTH (1+ I))
		     (LIM (// (SI:FEF-INITIAL-PC DEFN) 2)))
		    (( I LIM) NIL)
		  (COND ((= (%P-LDB-OFFSET %%Q-DATA-TYPE DEFN I)
			    DTP-EXTERNAL-VALUE-CELL-POINTER)
			 (SETQ TEM (%P-CONTENTS-AS-LOCATIVE-OFFSET DEFN I)
			       SYM (%FIND-STRUCTURE-HEADER TEM))
			 (AND (OR (EQ SYM FUNCTION)
				  (AND (EQ FUNCTION 'UNBOUND-FUNCTION)
				       (=  (%POINTER-DIFFERENCE TEM SYM) 2)
				       (NOT (FBOUNDP SYM))))
			      (PUSH CALLER LIST)))
			((EQ (%P-CONTENTS-OFFSET DEFN I) FUNCTION)
			 (PUSH CALLER LIST))))
		(AND (SI:FEF-CALLS-MISC-FUNCTION DEFN FUNCTION)
		     (PUSH CALLER LIST)))))))

(DEFUN LIST-CALLERS-1 (CALLER FUNCTION DEFN)
  (CATCH (LIST-CALLERS-2 DEFN FUNCTION CALLER) LIST-CALLERS-1))

(DEFUN LIST-CALLERS-2 (SUBLIST FUNCTION CALLER)
  (COND ((SYMBOLP SUBLIST)
	 (COND ((EQ SUBLIST FUNCTION)
		(PUSH CALLER LIST)
		(THROW NIL LIST-CALLERS-1))))
	((LISTP SUBLIST)
	 (LIST-CALLERS-2 (CAR SUBLIST) FUNCTION CALLER)
	 (LIST-CALLERS-2 (CDR SUBLIST) FUNCTION CALLER))))

(DEFUN LIST-MATCHING-SYMBOLS (FUNCTION &OPTIONAL (PKG PKG-GLOBAL-PACKAGE) &AUX LIST)
  (FUNCALL (IF (EQ PKG PKG-GLOBAL-PACKAGE) #'MAPATOMS-ALL #'MAPATOMS)
	   #'(LAMBDA (SYM) (AND (FUNCALL FUNCTION SYM) (PUSH SYM LIST)))
	   PKG)
  LIST)
);LOCAL-DECLARE

;;; Interval sorting
(DEFUN SORT-LINES-INTERVAL (LESSP-FN FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  "Given a lessp predicate and an interval, sort the lines in that interval.
The argument BP's are assumed to point at the beginning of their lines.
BP's to the ends of the interval remain at the ends of the interval, BP's
inside the interval move with their lines."
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (MUNG-BP-INTERVAL FROM-BP)
  (LET ((PRECEDING-LINE (LINE-PREVIOUS (BP-LINE FROM-BP)))
	(FOLLOWING-LINE (BP-LINE TO-BP))
	(PRECEDING-BPS (DO ((L (LINE-BP-LIST (BP-LINE FROM-BP)) (CDR L))
			    (R NIL))
			   ((NULL L) R)
			 (AND (ZEROP (BP-INDEX (CAR L)))
			      (EQ (BP-STATUS (CAR L)) ':NORMAL)
			      (PUSH (CAR L) R))))
	(N-LINES (1- (COUNT-LINES FROM-BP TO-BP T)))
	LINE-ARRAY FIRST-LINE)
    (SETQ LINE-ARRAY (MAKE-ARRAY NIL 'ART-Q N-LINES))
    (DO ((I 0 (1+ I))
	 (L (BP-LINE FROM-BP) (LINE-NEXT L)))
	((EQ L FOLLOWING-LINE))
      (ASET L LINE-ARRAY I))
    (SORT LINE-ARRAY LESSP-FN)
    (DO ((PREC PRECEDING-LINE LINE)
	 (I 0 (1+ I))
	 (LINE))
	((= I N-LINES)
	 (COND ((NOT (NULL LINE))
		(SETF (LINE-NEXT LINE) FOLLOWING-LINE)
		(SETF (LINE-PREVIOUS FOLLOWING-LINE) LINE))))
      (SETQ LINE (AREF LINE-ARRAY I))
      (AND PREC (SETF (LINE-NEXT PREC) LINE))
      (SETF (LINE-PREVIOUS LINE) PREC))
    (SETQ FIRST-LINE (AND (PLUSP N-LINES) (AREF LINE-ARRAY 0)))
    (RETURN-ARRAY (PROG1 LINE-ARRAY (SETQ LINE-ARRAY NIL)))
    (DOLIST (BP PRECEDING-BPS)
      (MOVE-BP BP FIRST-LINE 0))))

(DEFUN SORT-INTERVAL-ARRAY (ARRAY LESSP-FN FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (SORT ARRAY LESSP-FN)
  (LET ((NEW-INTERVAL (CREATE-INTERVAL)))
    (DO ((END-BP (INTERVAL-LAST-BP NEW-INTERVAL))
	 (I 0 (1+ I))
	 (LEN (ARRAY-ACTIVE-LENGTH ARRAY)))
	(( I LEN))
      (INSERT-INTERVAL END-BP (AREF ARRAY I)))
    (UNDO-SAVE FROM-BP TO-BP T "Sort")
    (DELETE-INTERVAL FROM-BP TO-BP T)
    (INSERT-INTERVAL FROM-BP NEW-INTERVAL)))

(DEFUN INTERVAL-LESSP (INTERVAL-1-FROM-BP INTERVAL-1-TO-BP INTERVAL-1-IN-ORDER-P
		       INTERVAL-2-FROM-BP INTERVAL-2-TO-BP INTERVAL-2-IN-ORDER-P)
  (GET-INTERVAL INTERVAL-1-FROM-BP INTERVAL-1-TO-BP INTERVAL-1-IN-ORDER-P)
  (GET-INTERVAL INTERVAL-2-FROM-BP INTERVAL-2-TO-BP INTERVAL-2-IN-ORDER-P)
  (DO ((LINE-1 (BP-LINE INTERVAL-1-FROM-BP))
       (LINE-2 (BP-LINE INTERVAL-2-FROM-BP))
       (LEN-1 (LINE-LENGTH (BP-LINE INTERVAL-1-FROM-BP)))
       (LEN-2 (LINE-LENGTH (BP-LINE INTERVAL-2-FROM-BP)))
       (INDEX-1 (BP-INDEX INTERVAL-1-FROM-BP) (1+ INDEX-1))
       (INDEX-2 (BP-INDEX INTERVAL-2-FROM-BP) (1+ INDEX-2))
       (END-LINE-1 (BP-LINE INTERVAL-1-TO-BP))
       (END-LINE-2 (BP-LINE INTERVAL-2-TO-BP))
       (END-INDEX-1 (BP-INDEX INTERVAL-1-TO-BP))
       (END-INDEX-2 (BP-INDEX INTERVAL-2-TO-BP))
       (CH-1) (CH-2))
      (NIL)
    ;; If the second string is exhausted, then the strings are equal or the second one is less
    ;; so we return false.
    (AND (EQ LINE-2 END-LINE-2) (= INDEX-2 END-INDEX-2)
	 (RETURN NIL))
    ;; If the first string is exhausted, it is less.
    (AND (EQ LINE-1 END-LINE-1) (= INDEX-1 END-INDEX-1)
	 (RETURN T))
    (IF (= INDEX-1 LEN-1)
	(SETQ LINE-1 (LINE-NEXT LINE-1)
	      LEN-1 (LINE-LENGTH LINE-1)
	      INDEX-1 -1
	      CH-1 #\CR)
	(SETQ CH-1 (AREF LINE-1 INDEX-1)))
    (IF (= INDEX-2 LEN-2)
	(SETQ LINE-2 (LINE-NEXT LINE-2)
	      LEN-2 (LINE-LENGTH LINE-2)
	      INDEX-2 -1
	      CH-2 #\CR)
	(SETQ CH-2 (AREF LINE-2 INDEX-2)))
    (AND (CHAR-LESSP CH-2 CH-1)
	 (RETURN NIL))
    (AND (CHAR-LESSP CH-1 CH-2)
	 (RETURN T))))

(DEFSTRUCT (INTERVAL-WITH-SORT-INTERVAL :ARRAY (:INCLUDE INTERVAL))
  INTERVAL-SORT-FIRST-BP
  INTERVAL-SORT-LAST-BP)

(DEFUN INTERVAL-WITH-SORT-INTERVAL-LESSP (INT1 INT2)
  (INTERVAL-LESSP (INTERVAL-SORT-FIRST-BP INT1) (INTERVAL-SORT-LAST-BP INT1) T
		  (INTERVAL-SORT-FIRST-BP INT2) (INTERVAL-SORT-LAST-BP INT2) T))

(DEFUN SORT-INTERVAL-FUNCTIONS (MOVE-TO-KEY-FN MOVE-OVER-KEY-FN MOVE-TO-NEXT-FN LESSP-FN
				FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (LET ((ARRAY (MAKE-ARRAY NIL 'ART-Q 20. NIL '(0))))
    (DO ((*INTERVAL* (CREATE-INTERVAL (COPY-BP FROM-BP ':NORMAL) (COPY-BP TO-BP ':MOVES)))
	 (START-BP FROM-BP END-BP)
	 (KEY-START-BP) (KEY-END-BP) (END-BP))
	((BP-= START-BP TO-BP))
      (SETQ KEY-START-BP (FUNCALL MOVE-TO-KEY-FN START-BP)
	    KEY-END-BP (FUNCALL MOVE-OVER-KEY-FN KEY-START-BP)
	    END-BP (FUNCALL MOVE-TO-NEXT-FN KEY-END-BP))
      (ARRAY-PUSH ARRAY (MAKE-INTERVAL-WITH-SORT-INTERVAL INTERVAL-FIRST-BP START-BP
							  INTERVAL-LAST-BP END-BP
							  INTERVAL-SORT-FIRST-BP KEY-START-BP
							  INTERVAL-SORT-LAST-BP KEY-END-BP)))
    (SORT-INTERVAL-ARRAY ARRAY LESSP-FN FROM-BP TO-BP T)))

(DEFSTRUCT (INTERVAL-WITH-SORT-KEY :ARRAY (:INCLUDE INTERVAL))
  INTERVAL-SORT-KEY)

(DEFUN SORT-INTERVAL-FUNCTIONS-WITH-KEY (MOVE-TO-KEY-FN GET-KEY-FN MOVE-TO-NEXT-FN LESSP-FN
				FROM-BP &OPTIONAL TO-BP IN-ORDER-P)
  (GET-INTERVAL FROM-BP TO-BP IN-ORDER-P)
  (LET ((ARRAY (MAKE-ARRAY NIL 'ART-Q 20. NIL '(0))))
    (DO ((*INTERVAL* (CREATE-INTERVAL (COPY-BP FROM-BP ':NORMAL) (COPY-BP TO-BP ':MOVES)))
	 (START-BP FROM-BP END-BP)
	 (KEY-START-BP) (KEY-END-BP) (KEY) (END-BP))
	((BP-= START-BP TO-BP))
      (SETQ KEY-START-BP (FUNCALL MOVE-TO-KEY-FN START-BP))
      (MULTIPLE-VALUE (KEY-END-BP KEY)
	(FUNCALL GET-KEY-FN KEY-START-BP))
      (SETQ END-BP (FUNCALL MOVE-TO-NEXT-FN KEY-END-BP))
      (ARRAY-PUSH ARRAY (MAKE-INTERVAL-WITH-SORT-KEY INTERVAL-FIRST-BP START-BP
						     INTERVAL-LAST-BP END-BP
						     INTERVAL-SORT-KEY KEY)))
    (SORT-INTERVAL-ARRAY ARRAY LESSP-FN FROM-BP TO-BP T)))
