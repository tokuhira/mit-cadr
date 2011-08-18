;;; -*- Mode: LISP; Package: TV; Base: 8 -*-
;;;	** (c) Copyright 1980, 1981 Massachusetts Institute of Technology **

;;;Miscellaneous user functions
(DEFUN SCREEN-CLEAR (&OPTIONAL (SCREEN DEFAULT-SCREEN))
  "This function is obsolete, but may still be called."
  ;; It isn't really obsolete, the initialization right below calls it
  (WITHOUT-INTERRUPTS
    (PREPARE-SHEET (SCREEN)
      (%DRAW-RECTANGLE (SHEET-WIDTH SCREEN) (SHEET-HEIGHT SCREEN)
		       0 0
		       ALU-ANDCA SCREEN))
    (AND (FBOUNDP 'WHO-LINE-CLOBBERED)
	 (WHO-LINE-CLOBBERED))
    (AND (FBOUNDP 'SCREEN-MANAGE-FLUSH-KNOWLEDGE)
	 (SCREEN-MANAGE-FLUSH-KNOWLEDGE SCREEN))))

(DEFUN SCREEN-REDISPLAY (&OPTIONAL (TYPE ':COMPLETE-REDISPLAY) (SCREEN DEFAULT-SCREEN))
  (FUNCALL SCREEN ':REFRESH TYPE)
  (WHO-LINE-CLOBBERED))

(DEFMETHOD (SCREEN :BEEP) (&OPTIONAL BEEP-TYPE)
  "Beep the beeper."
  BEEP-TYPE  ;We wanted to make this soo hairy, that we punted until we could do it right
  (AND BEEP
       (WITHOUT-INTERRUPTS  ;otherwise might quit out and leave screen complemented
	 (OR (EQ BEEP ':BEEP) (COMPLEMENT-BOW-MODE SELF))
	 (IF (EQ BEEP ':FLASH)
	     (%BEEP 0 BEEP-DURATION)	;Delay same time without making any noise
	     (%BEEP BEEP-WAVELENGTH BEEP-DURATION))
	 (OR (EQ BEEP ':BEEP) (COMPLEMENT-BOW-MODE SELF)))))

(DEFMETHOD (SHEET :BEEP) (&OPTIONAL BEEP-TYPE)
  (AND SUPERIOR (FUNCALL SUPERIOR ':BEEP BEEP-TYPE)))

(DEFUN BEEP (&OPTIONAL BEEP-TYPE (STREAM TERMINAL-IO))
  (IF (MEMQ ':BEEP (FUNCALL STREAM ':WHICH-OPERATIONS))
      (FUNCALL STREAM ':BEEP BEEP-TYPE)
      (%BEEP BEEP-WAVELENGTH BEEP-DURATION)))

(DEFUN BLACK-ON-WHITE (&OPTIONAL (SCREEN DEFAULT-SCREEN))
  (%XBUS-WRITE (SCREEN-CONTROL-ADDRESS SCREEN)
               (LOGIOR 4 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN)))))

(DEFUN WHITE-ON-BLACK (&OPTIONAL (SCREEN DEFAULT-SCREEN))
  (%XBUS-WRITE (SCREEN-CONTROL-ADDRESS SCREEN)
               (LOGAND -5 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN))))) ;1's comp of 4

(DEFUN COMPLEMENT-BOW-MODE (&OPTIONAL (SCREEN DEFAULT-SCREEN))
  (%XBUS-WRITE (SCREEN-CONTROL-ADDRESS SCREEN)
               (LOGXOR 4 (%XBUS-READ (SCREEN-CONTROL-ADDRESS SCREEN)))))

(DEFUN SHEET-INCREMENT-BITPOS (SHEET DX DY &AUX X Y MORE-VPOS)
  "Increment cursor X and cursor Y, keeping within sheet.  Sets exception flags
according to new positions"
  (SETF (SHEET-CURSOR-X SHEET)
	(SETQ X (MAX (+ DX (SHEET-CURSOR-X SHEET)) (SHEET-INSIDE-LEFT SHEET))))
  (SETF (SHEET-CURSOR-Y SHEET)
	(SETQ Y (MAX (+ DY (SHEET-CURSOR-Y SHEET)) (SHEET-INSIDE-TOP SHEET))))
  (AND (> (+ Y (SHEET-LINE-HEIGHT SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
       (SETF (SHEET-END-PAGE-FLAG SHEET) 1))
  (AND (SETQ MORE-VPOS (SHEET-MORE-VPOS SHEET))
       ( Y MORE-VPOS)
       (SETF (SHEET-MORE-FLAG SHEET) 1))
  NIL)

(DEFUN SHEET-TAB (SHEET)
  "Output a tab to a sheet"
  (PREPARE-SHEET (SHEET)
     (OR (ZEROP (SHEET-EXCEPTIONS SHEET)) (SHEET-HANDLE-EXCEPTIONS SHEET))
     (LET ((TAB-WIDTH (SHEET-TAB-WIDTH SHEET)))
       (SHEET-INCREMENT-BITPOS SHEET (- TAB-WIDTH (\ (- (SHEET-CURSOR-X SHEET)
							(SHEET-INSIDE-LEFT SHEET))
						     TAB-WIDTH))
			       0))))

(DEFUN SHEET-SET-FONT (SHEET FONT)
  "Change a sheet's current font"
  (SETF (SHEET-CURRENT-FONT SHEET) FONT)
  (SETF (SHEET-BASELINE-ADJ SHEET) (- (SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))

(DEFUN SHEET-SET-CURSORPOS (SHEET X Y)
  "Set 'cursor' position of a sheet in terms of raster units.  Cursorposes are relative
to the left and top margins.  Cursorpos is `clipped' to stay inside the sheet-inside."
  (DO ((INHIBIT-SCHEDULING-FLAG T T)  ;Keep trying until we get the lock
       (LOCK) (BL))
      ((AND (SETQ LOCK (SHEET-CAN-GET-LOCK SHEET))
	    (NOT (SHEET-OUTPUT-HELD-P SHEET)))
       (SETQ X (IF X (MIN (+ (MAX (FIX X) 0) (SHEET-INSIDE-LEFT SHEET))
			  (SHEET-INSIDE-RIGHT SHEET))
		   (SHEET-CURSOR-X SHEET)))
       (SETQ Y (IF Y (MIN (+ (MAX (FIX Y) 0) (SHEET-INSIDE-TOP SHEET))
			  (SHEET-INSIDE-BOTTOM SHEET))
		   (SHEET-CURSOR-Y SHEET)))
       (AND (= (SHEET-CURSOR-X SHEET) X) (= (SHEET-CURSOR-Y SHEET) Y)
	    (RETURN NIL))			;Not moving, don't open the blinker
       (AND (SETQ BL (SHEET-FOLLOWING-BLINKER SHEET))
	    (OPEN-BLINKER BL))
       (AND (SHEET-MORE-VPOS SHEET)		;If more processing enabled, delay until
						; bottom of sheet
	    (SETF (SHEET-MORE-VPOS SHEET) (SHEET-DEDUCE-MORE-VPOS SHEET)))
       (SETF (SHEET-CURSOR-X SHEET) X)
       (SETF (SHEET-CURSOR-Y SHEET) Y)
       (SETF (SHEET-EXCEPTIONS SHEET) 0)
       (AND (> (+ Y (SHEET-LINE-HEIGHT SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
	    (SETF (SHEET-END-PAGE-FLAG SHEET) 1))
       T)
    (SETQ INHIBIT-SCHEDULING-FLAG NIL)
    (IF LOCK
	(FUNCALL SHEET ':OUTPUT-HOLD-EXCEPTION)
	(PROCESS-WAIT "Lock" #'SHEET-CAN-GET-LOCK SHEET))))

(DEFUN SHEET-READ-CURSORPOS (SHEET)
  "Read the cursor position in raster units relative to margins"
  (VALUES (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
	  (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET))))

(DEFUN SHEET-HOME (SHEET)
  "Go to upper left edge of sheet (Home up)"
  (PREPARE-SHEET (SHEET)
    (AND (SHEET-MORE-VPOS SHEET)		;If MORE processing, put it off 'til last line
	 (SETF (SHEET-MORE-VPOS SHEET) (SHEET-DEDUCE-MORE-VPOS SHEET)))
    (SETF (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
    (SETF (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET))
    (SETF (SHEET-EXCEPTIONS SHEET) 0)))

(DEFUN SHEET-CRLF (SHEET)
  "Crlf and clear next line"
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))	;Handle exceptions first
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (SETF (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))
    (SHEET-INCREMENT-BITPOS SHEET 0 (SHEET-LINE-HEIGHT SHEET))
    (SHEET-CLEAR-EOL SHEET)))

(DEFUN SHEET-SPACE (SHEET &OPTIONAL CHAR)
  "Space forward"
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (SHEET-INCREMENT-BITPOS SHEET
			    (IF CHAR (SHEET-CHARACTER-WIDTH SHEET CHAR
							    (SHEET-CURRENT-FONT SHEET))
				     (SHEET-CHAR-WIDTH SHEET))
			    0)))

(DEFUN SHEET-BACKSPACE (SHEET &OPTIONAL CHAR)
  "Space backwards"
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (SHEET-INCREMENT-BITPOS SHEET
			    (- (IF CHAR (SHEET-CHARACTER-WIDTH SHEET CHAR
							       (SHEET-CURRENT-FONT SHEET))
					(SHEET-CHAR-WIDTH SHEET)))
			    0)))

(DEFUN SHEET-CLEAR-CHAR (SHEET &OPTIONAL CHAR)
  "Clear current character position"
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (%DRAW-RECTANGLE (IF CHAR (SHEET-CHARACTER-WIDTH SHEET CHAR
						     (SHEET-CURRENT-FONT SHEET))
			      (SHEET-CHAR-WIDTH SHEET))
		     (SHEET-LINE-HEIGHT SHEET)
		     (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
		     (SHEET-ERASE-ALUF SHEET) SHEET)))

(DEFUN SHEET-CLEAR-EOL (SHEET)
  "Clear to end of current line"
  (PREPARE-SHEET (SHEET)
    ;; Note that this need not handle **MORE** exception, because the **more**
    ;; would bash the line this is clearing anyway.  We don't want to **more**
    ;; if the next operation is going to be tyi.
    (OR (ZEROP (SHEET-END-PAGE-FLAG SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (%DRAW-RECTANGLE (MAX (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CURSOR-X SHEET))
			  0)
		     (MIN (- (SHEET-INSIDE-BOTTOM SHEET) (SHEET-CURSOR-Y SHEET))
			  (SHEET-LINE-HEIGHT SHEET))
		     (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
		     (SHEET-ERASE-ALUF SHEET) SHEET)))

(DEFMETHOD (SHEET :CLEAR-BETWEEN-CURSORPOSES) (START-X START-Y END-X END-Y)
  (SHEET-CLEAR-BETWEEN-CURSORPOSES SELF START-X START-Y END-X END-Y))

(DEFUN SHEET-CLEAR-BETWEEN-CURSORPOSES (SHEET START-X START-Y END-X END-Y
					&AUX (ALUF (SHEET-ERASE-ALUF SHEET)) MID-Y)
  "Erase from starting pos to ending pos
   Does nothing if start is after end on the same line, but if on different
   lines, assumes screen wrap-around"
  (SETQ START-X (MIN (+ START-X (SHEET-INSIDE-LEFT SHEET)) (SHEET-INSIDE-RIGHT SHEET))
	START-Y (MIN (+ START-Y (SHEET-INSIDE-TOP SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
	END-X (MIN (+ END-X (SHEET-INSIDE-LEFT SHEET)) (SHEET-INSIDE-RIGHT SHEET))
	END-Y (MIN (+ END-Y (SHEET-INSIDE-TOP SHEET)) (SHEET-INSIDE-BOTTOM SHEET)))
  (PREPARE-SHEET (SHEET)
    (COND ((= START-Y END-Y)
	   (COND ((< START-X END-X)
		  (%DRAW-RECTANGLE (- END-X START-X)
				   (MIN (- (SHEET-INSIDE-BOTTOM SHEET) START-Y)
					(SHEET-LINE-HEIGHT SHEET))
				   START-X START-Y ALUF SHEET))))
	  (T (%DRAW-RECTANGLE (- (SHEET-INSIDE-RIGHT SHEET) START-X) 
			      (MIN (- (SHEET-INSIDE-BOTTOM SHEET) START-Y)
				   (SHEET-LINE-HEIGHT SHEET))
			      START-X START-Y ALUF SHEET)
	     (SETQ MID-Y (+ START-Y (SHEET-LINE-HEIGHT SHEET)))
	     (%DRAW-RECTANGLE END-X (MIN (- (SHEET-INSIDE-BOTTOM SHEET) END-Y)
					 (SHEET-LINE-HEIGHT SHEET))
			      (SHEET-INSIDE-LEFT SHEET) END-Y ALUF SHEET)
	     (IF (< START-Y END-Y)
		 (AND (< MID-Y END-Y)
		      (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) (- END-Y MID-Y)
				       (SHEET-INSIDE-LEFT SHEET) MID-Y ALUF SHEET))
		 (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET)
				  (- (SHEET-INSIDE-BOTTOM SHEET) MID-Y)
				  (SHEET-INSIDE-LEFT SHEET) MID-Y ALUF SHEET)
		 (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET)
				  (- END-Y (SHEET-INSIDE-TOP SHEET))
				  (SHEET-INSIDE-LEFT SHEET) (SHEET-INSIDE-TOP SHEET)
				  ALUF SHEET))))))

(DEFMETHOD (SHEET :CLEAR-SCREEN) ()
  (SHEET-CLEAR SELF))

(DEFUN SHEET-CLEAR (SHEET &OPTIONAL (MARGINS-P NIL))
  (PREPARE-SHEET (SHEET)
    (SHEET-HOME SHEET)				;Handles any exceptions
    (IF MARGINS-P
	(%DRAW-RECTANGLE (SHEET-WIDTH SHEET) (SHEET-HEIGHT SHEET)
			 0 0
			 (SHEET-ERASE-ALUF SHEET) SHEET)
	(%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) (SHEET-INSIDE-HEIGHT SHEET)
			 (SHEET-INSIDE-LEFT SHEET) (SHEET-INSIDE-TOP SHEET)
			 (SHEET-ERASE-ALUF SHEET) SHEET))
    (SCREEN-MANAGE-FLUSH-KNOWLEDGE SHEET)))

(DEFUN SHEET-CLEAR-EOF (SHEET &AUX HT TEM)
  "Clear from cursor to end of sheet"
  (PREPARE-SHEET (SHEET)
    (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	(SHEET-HANDLE-EXCEPTIONS SHEET))
    (SHEET-CLEAR-EOL SHEET)
    (AND (PLUSP (SETQ HT (- (SHEET-INSIDE-BOTTOM SHEET)
			    (SETQ TEM (+ (SHEET-CURSOR-Y SHEET) (SHEET-LINE-HEIGHT SHEET))))))
	 (%DRAW-RECTANGLE (SHEET-INSIDE-WIDTH SHEET) HT
			  (SHEET-INSIDE-LEFT SHEET) TEM
			  (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFUN SHEET-HOME-DOWN (SHEET)
  "Place cursor at bottom of sheet"
  (SHEET-SET-CURSORPOS SHEET 0 (- (SHEET-INSIDE-HEIGHT SHEET) (SHEET-LINE-HEIGHT SHEET))))

(DEFUN SHEET-INSERT-LINE (SHEET &OPTIONAL (LINE-COUNT 1))
  "Make room for a line before the line the cursor is currently on"
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	  (WIDTH (SHEET-INSIDE-WIDTH SHEET))
	  (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	  HEIGHT
	  DELTA-HEIGHT)
      (SETQ HEIGHT (* LINE-COUNT LINE-HEIGHT))
      ;; Compute minus height of block to BLT
      (SETQ DELTA-HEIGHT
	    (- HEIGHT (- (* LINE-HEIGHT (SHEET-NUMBER-OF-INSIDE-LINES SHEET))
			 (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET)))))
      (OR ( DELTA-HEIGHT 0)			;If some bits to move, move them
	  (BITBLT ALU-SETA
		  WIDTH DELTA-HEIGHT
		  ARRAY (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET)
		  ARRAY (SHEET-INSIDE-LEFT SHEET) (+ (SHEET-CURSOR-Y SHEET) HEIGHT)))
      (%DRAW-RECTANGLE WIDTH HEIGHT
		       (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET)
		       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFUN SHEET-DELETE-LINE (SHEET &OPTIONAL (LINE-COUNT 1))
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	  (WIDTH (SHEET-INSIDE-WIDTH SHEET))
	  (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	  HEIGHT
	  DELTA-HEIGHT)
      (SETQ HEIGHT (* LINE-COUNT LINE-HEIGHT))
      (AND (PLUSP (SETQ DELTA-HEIGHT
			(- (+ (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET)) HEIGHT)
			   (* LINE-HEIGHT (SHEET-NUMBER-OF-INSIDE-LINES SHEET)))))
	   (FERROR NIL "Illegal line-count ~S for ~S" LINE-COUNT SHEET))
      (BITBLT ALU-SETA WIDTH (- DELTA-HEIGHT)
	      ARRAY (SHEET-INSIDE-LEFT SHEET) (+ (SHEET-CURSOR-Y SHEET) HEIGHT)
	      ARRAY (SHEET-INSIDE-LEFT SHEET) (SHEET-CURSOR-Y SHEET))
      (%DRAW-RECTANGLE WIDTH HEIGHT
		       (SHEET-INSIDE-LEFT SHEET) (- (SHEET-CURSOR-Y SHEET) DELTA-HEIGHT)
		       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFUN SHEET-INSERT-CHAR (SHEET &OPTIONAL (CHAR-COUNT 1) (TYPE ':CHARACTER))
  "Make room for characters after cursor.  Is only correct for fixed width fonts"
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	  (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	  (WIDTH (IF (EQ TYPE ':PIXEL) CHAR-COUNT
		     (* CHAR-COUNT (SHEET-CHAR-WIDTH SHEET)))))
      (BITBLT ALU-SETA
	      (- WIDTH (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CURSOR-X SHEET)))
	      LINE-HEIGHT
	      ARRAY (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
	      ARRAY (+ (SHEET-CURSOR-X SHEET) WIDTH) (SHEET-CURSOR-Y SHEET))
      (%DRAW-RECTANGLE WIDTH LINE-HEIGHT
		       (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET)
		       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFUN SHEET-DELETE-CHAR (SHEET &OPTIONAL (CHAR-COUNT 1) (TYPE ':CHARACTER))
  "Delete characters after cursor.  Is only correct for fixed width fonts"
  (PREPARE-SHEET (SHEET)
    (LET ((ARRAY (SHEET-SCREEN-ARRAY SHEET))
	  (LINE-HEIGHT (SHEET-LINE-HEIGHT SHEET))
	  (WIDTH (IF (EQ TYPE ':PIXEL) CHAR-COUNT
		     (* CHAR-COUNT (SHEET-CHAR-WIDTH SHEET)))))
      (BITBLT ALU-SETA
	      (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CURSOR-X SHEET) WIDTH)
	      LINE-HEIGHT
	      ARRAY (+ (SHEET-CURSOR-X SHEET) WIDTH) (SHEET-CURSOR-Y SHEET)
	      ARRAY (SHEET-CURSOR-X SHEET) (SHEET-CURSOR-Y SHEET))
      (%DRAW-RECTANGLE WIDTH LINE-HEIGHT
		       (- (SHEET-INSIDE-RIGHT SHEET) WIDTH)
		       (SHEET-CURSOR-Y SHEET)
		       (SHEET-ERASE-ALUF SHEET) SHEET))))

(DEFUN SHEET-INSERT-STRING (SHEET STRING &OPTIONAL (START 0) END (TYPE-TOO T) &AUX LEN)
  (SETQ LEN (IF (NUMBERP STRING)
		(SHEET-CHARACTER-WIDTH SHEET STRING (SHEET-CURRENT-FONT SHEET))
		(SHEET-STRING-LENGTH SHEET STRING START END)))
  (SHEET-INSERT-CHAR SHEET LEN ':PIXEL)
  (AND TYPE-TOO (SHEET-STRING-OUT SHEET STRING START END)))

(DEFUN SHEET-DELETE-STRING (SHEET STRING &OPTIONAL (START 0) END &AUX LEN)
  (SETQ LEN (IF (NUMBERP STRING)
		(SHEET-CHARACTER-WIDTH SHEET STRING (SHEET-CURRENT-FONT SHEET))
		(SHEET-STRING-LENGTH SHEET STRING START END)))
  (SHEET-DELETE-CHAR SHEET LEN ':PIXEL))

(DEFUN SHEET-TYO (SHEET CHAR &OPTIONAL FONT &AUX BASE-ADJ)
  "Draw a printing character in a sheet, or execute a special function"
  (IF ( CHAR 200)
      (COND ((AND (= CHAR #\CR) (ZEROP (SHEET-CR-NOT-NEWLINE-FLAG SHEET)))
             (SHEET-CRLF SHEET))
            ((= CHAR #\TAB)
             (SHEET-TAB SHEET))
            ((AND (= CHAR #\BS) (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
             (SHEET-BACKSPACE SHEET))
            (T
	     (SHEET-DISPLAY-LOSENGED-STRING SHEET
		(STRING (OR (CAR (RASSOC CHAR SI:XR-SPECIAL-CHARACTER-NAMES))
			    (FORMAT NIL "~O" CHAR))))))
      (PREPARE-SHEET (SHEET)
        (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	    (SHEET-HANDLE-EXCEPTIONS SHEET))
	(IF FONT (SETQ BASE-ADJ (SHEET-BASELINE-ADJ SHEET))
	    (SETQ FONT (SHEET-CURRENT-FONT SHEET)
		  BASE-ADJ (- (SHEET-BASELINE SHEET) (FONT-BASELINE FONT))))
        (LET* ((CHAR-WIDTHS (FONT-CHAR-WIDTH-TABLE FONT))
	       (FIT (FONT-INDEXING-TABLE FONT))
	       (WIDTH)
	       (KERN 0)
	       (KERN-TABLE)
	       (XPOS (SHEET-CURSOR-X SHEET))
	       (RIGHT-LIM (SHEET-INSIDE-RIGHT SHEET)))
	  (OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
	      (SETQ RIGHT-LIM (- RIGHT-LIM (SHEET-CHAR-WIDTH SHEET))))
	  (SETQ WIDTH (IF CHAR-WIDTHS
			  (AREF CHAR-WIDTHS CHAR)
			  (FONT-CHAR-WIDTH FONT)))
	  (COND ((> (+ XPOS WIDTH) RIGHT-LIM)
		 (FUNCALL SHEET ':END-OF-LINE-EXCEPTION)
		 (SHEET-TYO SHEET CHAR FONT))
		(T
		 (AND (SETQ KERN-TABLE (FONT-LEFT-KERN-TABLE FONT))
		      (SETQ KERN (AREF KERN-TABLE CHAR)))
		 (COND ((NULL FIT)
			(%DRAW-CHAR FONT CHAR (- XPOS KERN)
				    (+ (SHEET-CURSOR-Y SHEET) BASE-ADJ)
				    (SHEET-CHAR-ALUF SHEET)
				    SHEET))
		       ;; Wide character, draw several columns
		       (T
			 (DO ((CH (AREF FIT CHAR) (1+ CH))
			      (LIM (AREF FIT (1+ CHAR)))
			      (BPP (SHEET-BITS-PER-PIXEL SHEET))
			      (XPOS (- XPOS KERN)
				    (+ XPOS (// (FONT-RASTER-WIDTH FONT) BPP)))
			      (YPOS (+ (SHEET-CURSOR-Y SHEET) BASE-ADJ))
			      (ALUF (SHEET-CHAR-ALUF SHEET)))
			     ((= CH LIM))
			   (%DRAW-CHAR FONT CH XPOS YPOS ALUF SHEET))))
		 (SETF (SHEET-CURSOR-X SHEET) (+ XPOS WIDTH)))))))
  CHAR)

(DEFUN SHEET-STRING-OUT (SHEET STRING &OPTIONAL (START 0) (END NIL))
       "Routine to print a string on a sheet. Understands format effectors (special
keys 200-237).  Optional starting and ending indicies may be supplied.  Default is
to output the whole string"
  (PREPARE-SHEET (SHEET)
    (AND (SYMBOLP STRING)		;Convert symbols to strings for output
	 (SETQ STRING (GET-PNAME STRING)))
    (PROG ((I START)
	   (N (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	   (FONT (SHEET-CURRENT-FONT SHEET))
	   XPOS YPOS XLIM ALUF WIDTH CH FWT LKT)
       TOP
	  (AND ( I N) (RETURN NIL))		        ;No exception if done anyway
	  (AND (NULL (FONT-INDEXING-TABLE FONT))
	       (GO EZ))					;Handle easy case fast
       HD (SHEET-TYO SHEET (AREF STRING I))
	  (AND (< (SETQ I (1+ I)) N)
	       (GO TOP))
	  (RETURN NIL)

       EZ (OR (ZEROP (SHEET-EXCEPTIONS SHEET))		;End of page, MORE
	      (SHEET-HANDLE-EXCEPTIONS SHEET))
	  (SETQ XPOS (SHEET-CURSOR-X SHEET)
		YPOS (+ (SHEET-CURSOR-Y SHEET) (SHEET-BASELINE-ADJ SHEET))
	        ALUF (SHEET-CHAR-ALUF SHEET)
		WIDTH (FONT-CHAR-WIDTH FONT)
		XLIM (SHEET-INSIDE-RIGHT SHEET))
	  (OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
	      (SETQ XLIM (- XLIM (SHEET-CHAR-WIDTH SHEET))))
	  (AND (OR (FONT-CHAR-WIDTH-TABLE FONT) (FONT-LEFT-KERN-TABLE FONT))
	       (GO VW))					;Variable-width is a little slower
       EZ1						;This is the fast loop
	  (COND ((< (SETQ CH (AREF STRING I)) 200)	;Printing char
		 (COND ((> (+ XPOS WIDTH) XLIM)		;Room for it before right margin?
			(SETF (SHEET-CURSOR-X SHEET) XPOS)
			(FUNCALL SHEET ':END-OF-LINE-EXCEPTION)
			(GO TOP)))
		 (%DRAW-CHAR FONT CH XPOS YPOS ALUF SHEET)
		 (SETQ XPOS (+ XPOS WIDTH))
		 (AND (< (SETQ I (1+ I)) N)
		      (GO EZ1))
		 (SETF (SHEET-CURSOR-X SHEET) XPOS)
		 (RETURN NIL))
		(T					;Format effector
		 (SETF (SHEET-CURSOR-X SHEET) XPOS)
		 (GO HD)))

       VW  (SETQ FWT (FONT-CHAR-WIDTH-TABLE FONT)	;This is the medium speed loop 
		 LKT (FONT-LEFT-KERN-TABLE FONT))
       VW1 (COND ((< (SETQ CH (AREF STRING I)) 200)	;Printing char
		  (AND FWT (SETQ WIDTH (AREF FWT CH)))
		  (COND ((> (+ WIDTH XPOS) XLIM)	;Room before margin?
			 (SETF (SHEET-CURSOR-X SHEET) XPOS)
			 (FUNCALL SHEET ':END-OF-LINE-EXCEPTION)
			 (GO TOP)))
		  (%DRAW-CHAR FONT CH (IF LKT (- XPOS (AREF LKT CH)) XPOS) YPOS ALUF SHEET)
		  (SETQ XPOS (+ XPOS WIDTH))
		  (AND (< (SETQ I (1+ I)) N)
		       (GO VW1))
		  (SETF (SHEET-CURSOR-X SHEET) XPOS)
		  (RETURN NIL))
		 (T					;Format effector
		  (SETF (SHEET-CURSOR-X SHEET) XPOS)
		  (GO HD))))))

;;; Editor's line redisplay primitive, output STRING from START to END,
;;; first setting position to (SET-XPOS,SET-YPOS) and doing a clear-eol
;;; DWIDTH is a special hack for DIS-LINE redisplay of italic fonts, it means
;;; draw an extra character starting one character back, since the clear-eol
;;; will have erased part of the last character where it sticks out past its width.
;;; (If this can really happen, it's going to mean trouble with the margins, too!)
;;; This function never does more than one line; it stops rather than wrapping around.
;;; If you put a carriage return in the string, above may not be true.
;;; Where this leaves the sheet's actual cursorpos is undefined (somewhere on the line)
(DEFUN SHEET-LINE-OUT (SHEET STRING &OPTIONAL (START 0) (END NIL) SET-XPOS SET-YPOS DWIDTH)
  (DECLARE (RETURN-LIST I XPOS));Returns index of next character to do and where cursor got to
 				;Except the first value can be incremented, to show that
				;the line was completed (as if it counted the carriage return)
  (PREPARE-SHEET (SHEET)
    (PROG ((I START)
	   (N (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	   (RIGHT-LIMIT (SHEET-INSIDE-RIGHT SHEET))
	   (MARGIN-FLAG (NOT (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))))
	   XPOS YPOS OYPOS ALUF WIDTH FWT LKT CH FONT FONTX TEM BASE-ADJ)
      (AND MARGIN-FLAG (SETQ RIGHT-LIMIT (- RIGHT-LIMIT (SHEET-CHAR-WIDTH SHEET))))
      (COND (SET-XPOS
	     (SETF (SHEET-CURSOR-X SHEET)
		   (SETQ SET-XPOS (MIN (+ SET-XPOS (SHEET-INSIDE-LEFT SHEET))
				       (SHEET-INSIDE-RIGHT SHEET))))))
      (COND (SET-YPOS
	     (AND (SHEET-MORE-VPOS SHEET)
		  (SETF (SHEET-MORE-VPOS SHEET) (SHEET-DEDUCE-MORE-VPOS SHEET)))
	     (SETF (SHEET-CURSOR-Y SHEET)
		   (SETQ SET-YPOS (MIN (+ SET-YPOS (SHEET-INSIDE-TOP SHEET))
				       (SHEET-INSIDE-BOTTOM SHEET))))
	     (SETF (SHEET-EXCEPTIONS SHEET) 0)
	     (AND (> (+ SET-YPOS (SHEET-LINE-HEIGHT SHEET)) (SHEET-INSIDE-BOTTOM SHEET))
		  (SETF (SHEET-END-PAGE-FLAG SHEET) 1))
	     (SETQ OYPOS SET-YPOS))
	    (T (SETQ OYPOS (SHEET-CURSOR-Y SHEET))))

      (OR (ZEROP (SHEET-EXCEPTIONS SHEET)) (SHEET-HANDLE-EXCEPTIONS SHEET))

      ;; If we set the cursor then do a clear to end of line
      (AND (OR SET-XPOS SET-YPOS)
	   (%DRAW-RECTANGLE (- (SHEET-INSIDE-RIGHT SHEET)
			       (SETQ SET-XPOS (OR SET-XPOS (SHEET-CURSOR-X SHEET))))
			    (SHEET-LINE-HEIGHT SHEET)
			    SET-XPOS OYPOS
			    (SHEET-ERASE-ALUF SHEET) SHEET))
      ;; If special case of italic line, move back and decrement starting index
      (COND (DWIDTH
	     (SETF (SHEET-CURSOR-X SHEET) (- SET-XPOS DWIDTH))
	     (SETQ I (1- I))))

  HD  (AND ( I N)
	   (RETURN (1+ I) (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))))
      (SETQ CH (AREF STRING I))
      (OR (EQ (SETQ TEM (LDB %%CH-FONT CH)) FONTX)	;Changing to a new font
	  (LET ((FONT-MAP (SHEET-FONT-MAP SHEET)))
	    (SETQ FONTX TEM)
	    (SETQ FONT (AREF FONT-MAP (IF ( FONTX (ARRAY-ACTIVE-LENGTH FONT-MAP)) 0 FONTX))
		  BASE-ADJ (- (SHEET-BASELINE SHEET) (FONT-BASELINE FONT)))))
  HD1 (SETQ WIDTH (SHEET-CHARACTER-WIDTH SHEET (SETQ CH (LDB %%CH-CHAR CH)) FONT))
      (COND ((> (+ (SHEET-CURSOR-X SHEET) WIDTH) RIGHT-LIMIT)	;This char won't fit
	     (AND MARGIN-FLAG (SHEET-TYO-RIGHT-MARGIN-CHARACTER SHEET
						(SHEET-CURSOR-X SHEET) OYPOS #/!))
	     (RETURN (IF (ZEROP (SHEET-TRUNCATE-LINE-OUT-FLAG SHEET)) I (1+ N))
		     (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET)))))
      (COND ((AND (< CH 200)
		  (NULL (FONT-INDEXING-TABLE FONT)))	;Let SHEET-TYO do big fonts
	     (AND (NULL (FONT-CHAR-WIDTH-TABLE FONT))
		  (NULL (FONT-LEFT-KERN-TABLE FONT))
		  (GO EZ))				;Handle easy fixed-width case fast
	     (GO VW)))					;Variable-width is a little slower
      (SHEET-TYO SHEET CH FONT)
      (SETQ I (1+ I))
      (GO HD)
  
      ;;This loop is for simple fonts that don't need full hair of SHEET-TYO
  EZ  (SETQ XPOS (SHEET-CURSOR-X SHEET)
	    ALUF (SHEET-CHAR-ALUF SHEET))
  EZ0 (SETQ WIDTH (FONT-CHAR-WIDTH FONT)
	    YPOS (+ (SHEET-CURSOR-Y SHEET) BASE-ADJ))
  EZ1 (OR (< CH 200) (GO EZX))				;Format effector, call full TYO
      (COND ((> (+ XPOS WIDTH) RIGHT-LIMIT)		;Form continuation line
	     (AND MARGIN-FLAG (SHEET-TYO-RIGHT-MARGIN-CHARACTER SHEET XPOS YPOS #/!))
	     (RETURN (IF (ZEROP (SHEET-TRUNCATE-LINE-OUT-FLAG SHEET)) I (1+ N))
		     (- XPOS (SHEET-INSIDE-LEFT SHEET)))))
      (%DRAW-CHAR FONT CH XPOS YPOS ALUF SHEET)
      (SETQ XPOS (+ XPOS WIDTH))
      ;;Get next character, if any left
      (COND (( (SETQ I (1+ I)) N)
	     (SETF (SHEET-CURSOR-X SHEET) XPOS)		;Necessary?
	     (RETURN (1+ I) (- XPOS (SHEET-INSIDE-LEFT SHEET)))))
      (SETQ CH (AREF STRING I)
	    TEM (LDB %%CH-FONT CH)
	    CH (LDB %%CH-CHAR CH))
      (AND (EQ TEM FONTX)
	   (GO EZ1))
      (SETQ FONT (AREF (SHEET-FONT-MAP SHEET) (SETQ FONTX TEM))	;Changing to a new font
	    BASE-ADJ (- (SHEET-BASELINE SHEET) (FONT-BASELINE FONT)))
      (AND (NULL (FONT-LEFT-KERN-TABLE FONT))
	   (NULL (FONT-INDEXING-TABLE FONT))
	   (NULL (FONT-CHAR-WIDTH-TABLE FONT))
	   (GO EZ0))					;Handle easy case fast
  EZX (SETF (SHEET-CURSOR-X SHEET) XPOS)
      (GO HD1)						;Go type out char and enter HD loop

      ;;This loop is for variable-width fonts
  VW  (SETQ XPOS (SHEET-CURSOR-X SHEET)
	    ALUF (SHEET-CHAR-ALUF SHEET)
	    FWT (FONT-CHAR-WIDTH-TABLE FONT)
	    LKT (FONT-LEFT-KERN-TABLE FONT)
	    YPOS (+ (SHEET-CURSOR-Y SHEET) BASE-ADJ))
  VW1 (OR (< CH 200) (GO EZX))				;Format effector, call full TYO
      (AND FWT (SETQ WIDTH (AREF FWT CH)))
      (COND ((> (+ WIDTH XPOS) RIGHT-LIMIT)		;Won't fit in line
	     (AND MARGIN-FLAG (SHEET-TYO-RIGHT-MARGIN-CHARACTER SHEET XPOS YPOS #/!))
	     (RETURN (IF (ZEROP (SHEET-TRUNCATE-LINE-OUT-FLAG SHEET)) I (1+ N))
		     (- XPOS (SHEET-INSIDE-LEFT SHEET)))))
      (%DRAW-CHAR FONT CH (IF LKT (- XPOS (AREF LKT CH)) XPOS) YPOS ALUF SHEET)
      (SETQ XPOS (+ XPOS WIDTH))
      ;;Get next character, if any left
      (COND (( (SETQ I (1+ I)) N)
	     (SETF (SHEET-CURSOR-X SHEET) XPOS)		;Necessary?
	     (RETURN (1+ I) (- XPOS (SHEET-INSIDE-LEFT SHEET)))))
      (SETQ CH (AREF STRING I)
	    TEM (LDB %%CH-FONT CH)
	    CH (LDB %%CH-CHAR CH))
      (AND (EQ TEM FONTX) (GO VW1))
      (SETF (SHEET-CURSOR-X SHEET) XPOS)
      (GO HD)
      )))

;; Compute the motion that would be caused by outputing a string.
;;  This is used by the editor and by TV:STREAM-MIXIN.
;; In computing the motion, it will chose the font in one of two ways:
;;  If given an ART-FAT-STRING array (16 bit string) like the editor uses,
;;  it will take the font from the %%CH-FONT field (high 8 bits) of the
;;  character.
;;  If given an ART-STRING array (8 bit string), it will take the font from
;;  SHEET-CURRENT-FONT of the sheet.
;; Args are: sheet, X and Y position to start at (NILs here use the current
;;  position of the sheet), string, and optionally the starting and ending indices
;;  and a flag saying to fake a CRLF at end of the string.
;;  Optionally you can give two additional arguments which are the X and Y to stop at,
;;  if not given these default to the end of the sheet.
;; Returns 3 values: FINAL-X, FINAL-Y, and an indication of how far down the
;;  string it got.  this is NIL if the whole string (including the fake
;;  carriage return, if any) was processed without
;;  reaching the stopping point, or the index of the next character to be
;;  processed when the stopping point was reached, or T if the stopping point
;;  was reached after the fake carriage return.
;; *** The interface to this crock should be redesigned.  Also note that the
;; *** exact treatment of STOP-X and STOP-Y does not agree with SHEET-STRING-LENGTH.

(DEFUN SHEET-COMPUTE-MOTION (SHEET X Y STRING
			     &OPTIONAL (START 0) (END NIL) (CR-AT-END-P NIL)
				       (STOP-X 0) (STOP-Y NIL) BOTTOM-LIMIT RIGHT-LIMIT)
  (PROG (CWA CW CH FONT FONTX TEM I N NN II MARGIN-FLAG)
    (OR (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET)) (SETQ MARGIN-FLAG T))
    (AND (NULL X) (SETQ X (- (SHEET-CURSOR-X SHEET) (SHEET-INSIDE-LEFT SHEET))))
    (AND (NULL Y) (SETQ Y (- (SHEET-CURSOR-Y SHEET) (SHEET-INSIDE-TOP SHEET))))
    (AND (NULL STOP-Y)
	 (SETQ STOP-Y (1+ (SHEET-INSIDE-HEIGHT SHEET))))
		    ;   ^-- THIS 1+ IS SO CAN USE  RATHER THAN >
    (OR RIGHT-LIMIT (SETQ RIGHT-LIMIT (SHEET-INSIDE-WIDTH SHEET)))
    (AND MARGIN-FLAG (SETQ RIGHT-LIMIT (- RIGHT-LIMIT (SHEET-CHAR-WIDTH SHEET))))
    (AND (NULL BOTTOM-LIMIT)
	 (SETQ BOTTOM-LIMIT (- (SHEET-INSIDE-HEIGHT SHEET) (SHEET-LINE-HEIGHT SHEET))))
    (SETQ I START
 	  N (OR END (ARRAY-ACTIVE-LENGTH STRING))
	  FONT (SHEET-CURRENT-FONT SHEET)
	  CW (FONT-CHAR-WIDTH FONT))
    ;; At this point, decide whether we can use the fast version.
    (COND
      ;; If FONTX is non-NIL, then we have a string with font changes.
      (( (%P-MASK-FIELD-OFFSET %%ARRAY-TYPE-FIELD STRING 0) ART-STRING)
       (SETQ FONTX T))
      ;; The current font is variable width.
      ((SETQ CWA (FONT-CHAR-WIDTH-TABLE FONT)))
      ;; No font changes and the current font is fixed width.  We can use the fast version.
      (T (GO FAST)))
    ;;This is the slow version.
SLOW
    (COND ((AND ( Y STOP-Y) ( X STOP-X))	;Reached sticking-point
	   (RETURN X Y I))
	  ((NOT (< I N))			;If string exhausted
	   (COND (CR-AT-END-P
		  (SETQ X 0 Y (+ Y (SHEET-LINE-HEIGHT SHEET)))	;CRLF if told to
		  (AND (> Y BOTTOM-LIMIT) (SETQ Y 0))))
	   (RETURN X Y (AND ( X STOP-X) ( Y STOP-Y)))))
    (SETQ CH (LDB %%CH-CHAR (SETQ TEM (AREF STRING I))))
    (COND ((AND FONTX (NEQ (SETQ TEM (LDB %%CH-FONT TEM)) FONTX)) ;Changing fonts
	   (SETQ FONTX TEM
		 FONT (LET ((FONT-MAP (SHEET-FONT-MAP SHEET)))
			(AREF FONT-MAP (IF ( FONTX (ARRAY-ACTIVE-LENGTH FONT-MAP))
					   0 FONTX)))
		 CWA (FONT-CHAR-WIDTH-TABLE FONT)
		 CW (FONT-CHAR-WIDTH FONT))))
    (COND ((= CH #\CR)
	   (SETQ X 0 Y (+ Y (SHEET-LINE-HEIGHT SHEET)))
	   (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	  ((< CH 200)				;Printing character
	   (SETQ X (+ (COND (CWA (AREF CWA CH)) (T CW)) X))) ;do char width
	  ((= CH #\TAB)				;Tab (have to do here since x-dependent)
	   (SETQ TEM (SHEET-TAB-WIDTH SHEET))
	   (SETQ X (* (// (+ X TEM) TEM) TEM)))
	  (T					;Format effector
	   (SETQ X (MAX (+ X (SHEET-CHARACTER-WIDTH SHEET CH FONT)) 0))))
    (COND ((> X RIGHT-LIMIT)			;If this character doesn't fit, crlf
	   (SETQ X 0 Y (+ Y (SHEET-LINE-HEIGHT SHEET)))	; and do it again
	   (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	  (T (SETQ I (1+ I))))
    (GO SLOW)

    ;;Here is the fast loop.  The basic idea is to scan as fast as possible
    ;;over printing characters, with all checking outside the loop.
FAST 
    ;;First, decide the most characters we want to scan over in a whack
    (SETQ NN (MIN (+ (// (- (COND (( Y STOP-Y)	;Stop-point is in this line
				   STOP-X)
				  (T RIGHT-LIMIT))	;Stop for this line is margin
			    X)
			 CW)
		     I)
		  N))				;NN is limiting value of I
    ;Now, scan over printing characters.
    (AND ( (SETQ II I) NN)			;Save initial I, and check for null loop
	 (GO SCX))
    (SETQ TEM 200)				;This is really a ridiculous bum
SCN (AND (< (AREF STRING I) TEM)		;If this is a printing character
	 (< (SETQ I (1+ I)) NN)			; and we haven't reached stop point
	 (GO SCN))				; then continue to loop (9 instructions)
    (SETQ X (+ (* (- I II) CW) X))		;Account for the motion of those chars
SCX (SETQ NN X)
    (COND ((AND ( Y STOP-Y) ( X STOP-X))	;If reached sticking-point, done.
	   (RETURN X Y I))
	  ((NOT (< I N))			;If string exhausted
	   (COND (CR-AT-END-P			;Do return X off end of line
		  (SETQ X 0 Y (+ Y (SHEET-LINE-HEIGHT SHEET)))	;crlf if told to
		  (AND (> Y BOTTOM-LIMIT) (SETQ Y 0))))
	   (RETURN X Y (AND ( X STOP-X) ( Y STOP-Y)))))
    (COND ((= (SETQ CH (AREF STRING I)) #\CR)
	   (SETQ X 0 Y (+ Y (SHEET-LINE-HEIGHT SHEET)))
	   (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	  ((< CH 200)				;Printing character
	   (SETQ X (+ CW X)))
	  ((= CH #\TAB)				;Tab (have to do here since x-dependent)
	   (SETQ TEM (SHEET-TAB-WIDTH SHEET))
	   (SETQ X (* (// (+ X TEM) TEM) TEM)))
	  (T					;Format effector
	   (SETQ X (MAX (+ X (SHEET-CHARACTER-WIDTH SHEET CH FONT)) 0))))
    (COND ((> X RIGHT-LIMIT)			;If this char didn't fit, crlf and do again
	   (SETQ X 0 Y (+ Y (SHEET-LINE-HEIGHT SHEET)))
	   (AND (> Y BOTTOM-LIMIT) (SETQ Y 0)))
	  (T (SETQ I (1+ I))))
    (GO FAST)
))

(DEFUN SHEET-CHARACTER-WIDTH (SHEET CH FONT &AUX TEM)
  "Returns the width of a character, in raster units.
For backspace, it can return a negative number.
For tab, the number returned depends on the current cursor position.
For return, the result is zero."
  (COND ((< CH 200)				;Ordinary printing character
	 (COND ((SETQ TEM (FONT-CHAR-WIDTH-TABLE FONT)) (AREF TEM CH))
	       (T (FONT-CHAR-WIDTH FONT))))
	((= CH #\CR) 0)				        ;Return
	((= CH #\TAB)				        ;Tab
	 (SETQ TEM (SHEET-TAB-WIDTH SHEET))
	 (- (* (// (+ (SHEET-CURSOR-X SHEET) TEM) TEM) TEM)
	    (SHEET-CURSOR-X SHEET)))
	((AND (= CH #\BS) (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
	 (MINUS (SHEET-CHAR-WIDTH SHEET)))		;Backspace
	(T						;Misc losenge character
	 (LET ((TEM (CAR (RASSOC CH SI:XR-SPECIAL-CHARACTER-NAMES))))
	   (SETQ TEM (IF TEM (STRING-LENGTH TEM) 3))
	   (+ (* TEM 6) 10.)))))

;;; This is like SHEET-COMPUTE-MOTION, but in one dimension only.
;;; Returned values are X, I, and MAX-X.  X is the cursor position when all
;;; characters are processed.  I is the index of the next
;;; character to be processed; I is the length of the string (or END) unless
;;; STOP-X is specified in which case it may be the index of the character
;;; which would have made the cursor > STOP-X.  MAX-X is the greatest value
;;; that X ever attained; it can be different from X if there are backspaces
;;; or newlines in the string.
(DEFUN SHEET-STRING-LENGTH (SHEET STRING &OPTIONAL (START 0) (END NIL) (STOP-X NIL)
						   FONT (START-X 0) (MAX-X 0))
  (OR FONT (SETQ FONT (SHEET-CURRENT-FONT SHEET)))
  (PROG (CWA CW CH FONTX TEM I N NN II STRINGP (X START-X))
    (SETQ I START
	  N (OR END (ARRAY-ACTIVE-LENGTH STRING))
	  CW (FONT-CHAR-WIDTH FONT))
    ;At this point, decide whether we can use the fast version
SLOW
    (AND (SETQ STRINGP (= (%P-MASK-FIELD-OFFSET %%ARRAY-TYPE-FIELD STRING 0)
			  ART-STRING))			;i.e. no font changes
	 (NULL (SETQ CWA (FONT-CHAR-WIDTH-TABLE FONT)))	;and fixed width
	 (GO FAST))
SLOW0
    (OR (< I N) (RETURN X I MAX-X))			;If string exhausted
    (SETQ CH (LDB %%CH-CHAR (SETQ TEM (AREF STRING I))))
    (COND ((AND (NOT STRINGP)				;Changing fonts
		(NEQ (SETQ TEM (LDB %%CH-FONT TEM)) FONTX))
	   (SETQ FONTX TEM
		 FONT (AREF (SHEET-FONT-MAP SHEET) FONTX)
		 CWA (FONT-CHAR-WIDTH-TABLE FONT)
		 CW (FONT-CHAR-WIDTH FONT))))
    (COND ((< CH 200)					;Printing character
	   (SETQ NN (IF CWA (AREF CWA CH) CW)))
	  ((= CH #\TAB)
	   (SETQ TEM (SHEET-TAB-WIDTH SHEET))
	   (SETQ NN (- (* (// (+ X TEM) TEM) TEM) X)))
	  ((AND (= CH #\BS) (ZEROP (SHEET-BACKSPACE-NOT-OVERPRINTING-FLAG SHEET)))
	   (SETQ NN (- (MAX 0 (- X (SHEET-CHAR-WIDTH SHEET))) X)))
	  ((= CH #\CR)
	   (SETQ NN 0 X 0))
	  (T					;Losenged character
	   (SETQ NN (SHEET-CHARACTER-WIDTH SHEET CH FONT))))
    (SETQ X (+ X NN))
    (IF (> X MAX-X) (SETQ MAX-X X))
    (AND STOP-X (> X STOP-X)			;If char doesn't fit, stop before it
	 (RETURN (- X NN) I MAX-X))
    (SETQ I (1+ I))
    (GO SLOW)

    ;Here is the fast loop.  The basic idea is to scan as fast as possible
    ;over printing characters, with all checking outside the loop.
FAST 
    ;First, decide the most characters we want to scan over in a whack
    (SETQ NN (COND ((NULL STOP-X) N)		;NN is limiting value of I
                   ((MIN (+ (// (- STOP-X X)
                                CW)
                            I)
                         N))))
    ;Now, scan over printing characters.
    (AND ( (SETQ II I) NN)			;Save initial I, and check for null loop
	 (GO SLOW0))
    (SETQ TEM 200)				;This is really a ridiculous bum
SCN (AND (< (AREF STRING I) TEM)		;If this is a printing character
	 (< (SETQ I (1+ I)) NN)			; and we haven't reached stop point
	 (GO SCN))				; then continue to loop (9 instructions)
    (SETQ X (+ (* (- I II) CW) X))		;Account for the motion of those chars
    (IF (> X MAX-X) (SETQ MAX-X X))
    (GO SLOW0)					;Either string exhausted, non-printing,
						; or reached stop-x
))

(DEFUN SHEET-STRING-OUT-EXPLICIT (SHEET STRING X Y XLIM FONT ALU
					&OPTIONAL (START 0) (END NIL)
					&AUX FIT FWT LKT)
  "Output a special string (like a label) without exceptions or anything like that."
  (SETQ FIT (FONT-INDEXING-TABLE FONT)
	FWT (FONT-CHAR-WIDTH-TABLE FONT)
	LKT (FONT-LEFT-KERN-TABLE FONT))
  (PREPARE-SHEET (SHEET)
    (DO ((I START (1+ I))
	 (N (OR END (ARRAY-ACTIVE-LENGTH STRING)))
	 (WIDTH (FONT-CHAR-WIDTH FONT))
	 (CH))
	(( I N) (VALUES X I))
      (SETQ CH (AREF STRING I))
      (COND (( CH 200)
	     (SETQ X (SHEET-DISPLAY-LOSENGED-STRING-INTERNAL SHEET
			(STRING (OR (CAR (RASSOC CH SI:XR-SPECIAL-CHARACTER-NAMES))
				    (FORMAT NIL "~O" CH)))
			X (1+ Y) XLIM ALU))
	     (IF (> X XLIM) (RETURN X I)))
	    (T (IF FWT (SETQ WIDTH (AREF FWT CH)))
	       (IF LKT (SETQ X (- X (AREF LKT CH))))
	       (IF (> (+ X WIDTH) XLIM) (RETURN X I))
	       (IF FIT
		   (DO ((CH (AREF FIT CH) (1+ CH))
			(LIM (AREF FIT (1+ CH)))
			(BPP (SHEET-BITS-PER-PIXEL SHEET))
			(X X (+ X (// (FONT-RASTER-WIDTH FONT) BPP))))
		       (( CH LIM))
		     (%DRAW-CHAR FONT CH X Y ALU SHEET))
		   (%DRAW-CHAR FONT CH X Y ALU SHEET))
	       (SETQ X (+ X WIDTH)))))))

;;; This function displays a string centered between two X coordinates, truncated if necessary
;;; The arguments are relative to the margins, as usual.
(DEFUN SHEET-DISPLAY-CENTERED-STRING (SHEET STRING
				      &OPTIONAL (LEFT 0) (RIGHT (SHEET-INSIDE-WIDTH SHEET))
				                (Y-POS (- (SHEET-CURSOR-Y SHEET)
							  (SHEET-INSIDE-TOP SHEET)))
				      &AUX WID SWID SLEN)
  (SETQ WID (- RIGHT LEFT)
	STRING (STRING STRING))
  (MULTIPLE-VALUE (SWID SLEN)  ;Compute how wide the string is, and whether to truncate
     (SHEET-STRING-LENGTH SHEET STRING 0 NIL WID))
  ;; SHEET-SET-CURSORPOS takes arguments in a different coordinate system
  (SHEET-SET-CURSORPOS SHEET (+ LEFT (MAX (// (- WID SWID) 2) 0)) Y-POS)
  (SHEET-STRING-OUT SHEET STRING 0 SLEN))

(DEFUN SHEET-DISPLAY-X-Y-CENTERED-STRING (SHEET STRING
					  &OPTIONAL (LEFT 0) (TOP 0)
					            (RIGHT (SHEET-INSIDE-WIDTH SHEET))
						    (BOTTOM (SHEET-INSIDE-HEIGHT SHEET))
						    (FNT (SHEET-CURRENT-FONT SHEET)))
  "Display a string centered in both X and Y.
  Note that the coordinates of the box in which it is centered are relative to the margins"
  (LET ((HT (FONT-BASELINE FNT))
	(WID (- RIGHT LEFT)))
    (MULTIPLE-VALUE-BIND (SWID SLEN)
	(SHEET-STRING-LENGTH SHEET STRING 0 NIL WID FNT)
      (SHEET-STRING-OUT-EXPLICIT SHEET STRING
				 (+ (SHEET-INSIDE-LEFT SHEET) LEFT
				    (MAX (// (- WID SWID) 2) 0))
				 (+ (SHEET-INSIDE-TOP SHEET)
				    (MAX (- (// (+ TOP BOTTOM) 2) (// HT 2)) TOP))
				 (+ (SHEET-INSIDE-LEFT SHEET) RIGHT)
				 FNT (SHEET-CHAR-ALUF SHEET) 0 SLEN))))

(DEFUN SHEET-DISPLAY-LOSENGED-STRING (SHEET STRING)
  (LET ((WIDTH (+ 10. (* (STRING-LENGTH STRING) 6))))
    ;; Make sure there is enough room on the line, if not CRLF and
    ;; hope the sheet isn't too narrow.  Relies on the fact that handling
    ;; of all exceptions leaves you no further to the right than you were
    ;; (usually at the left margin).
    (PREPARE-SHEET (SHEET)
      (OR (ZEROP (SHEET-EXCEPTIONS SHEET))
	  (SHEET-HANDLE-EXCEPTIONS SHEET))
      (COND ((> (+ (SHEET-CURSOR-X SHEET) WIDTH)
		(IF (ZEROP (SHEET-RIGHT-MARGIN-CHARACTER-FLAG SHEET))
		    (SHEET-INSIDE-RIGHT SHEET)
		    (- (SHEET-INSIDE-RIGHT SHEET) (SHEET-CHAR-WIDTH SHEET))))
	     (FUNCALL SHEET ':END-OF-LINE-EXCEPTION)))
      (SETF (SHEET-CURSOR-X SHEET)
	    (SHEET-DISPLAY-LOSENGED-STRING-INTERNAL SHEET STRING
			(SHEET-CURSOR-X SHEET) (1+ (SHEET-CURSOR-Y SHEET))
			(SHEET-INSIDE-RIGHT SHEET) (SHEET-CHAR-ALUF SHEET))))))

(DEFUN SHEET-DISPLAY-LOSENGED-STRING-INTERNAL (SHEET STRING X0 Y0 XLIM ALUF)
  (LET ((WIDTH (+ 10. (* (STRING-LENGTH STRING) 6))))
    ;; Put the string then the box around it
    (LET ((X1 (1- (MIN (+ X0 WIDTH) XLIM)))
	  (Y1 (+ Y0 8)))
      (SHEET-STRING-OUT-EXPLICIT SHEET STRING (+ X0 5) (+ Y0 2)
				 X1
				 (FUNCALL (SHEET-GET-SCREEN SHEET)
					  ':PARSE-FONT-DESCRIPTOR FONTS:5X5)
				 ALUF)
      (%DRAW-RECTANGLE (- WIDTH 8) 1 (+ X0 4) Y0 ALUF SHEET)
      (%DRAW-RECTANGLE (- WIDTH 8) 1 (+ X0 4) Y1 ALUF SHEET)
      (%DRAW-LINE X0 (+ Y0 4) (+ X0 3) (1+ Y0) ALUF T SHEET)
      (%DRAW-LINE (1+ X0) (+ Y0 5) (+ X0 3) (1- Y1) ALUF T SHEET)
      (%DRAW-LINE X1 (+ Y0 4) (- X1 3) (1+ Y0) ALUF T SHEET)
      (%DRAW-LINE (1- X1) (+ Y0 5) (- X1 3) (1- Y1) ALUF T SHEET)
      (1+ X1))))

;;; Most screens can be seen by the "user"
(DEFMETHOD (SCREEN :USER-VISIBLE) () T)

;;; A mixin that causes inferiors to be scaled when the size of the window changes
;;; and propagates changes in the default font.
;;; TIME-STAMP is (as for any sheet), the time-stamp for comparison with this sheet's superior
;;; CURRENT-TIME-STAMP is the stamp which propagates down into our inferiors.  If
;;; an inferior's TIME-STAMP is EQ to our CURRENT-TIME-STAMP, then the inferior is
;;; up to date.  Otherwise we compare the two stamps and resolve the differences.
;;; This comparison happens to the active inferiors when our stamp changes and
;;; to any newly-activated inferior.
;;; This mixin is the only thing which knows the format of time stamps (other than
;;; that they are compared with EQ).  A time stamp is a list which represents
;;; the state of a window that has this mixin:
;;;	(serial-number our-inside-width our-inside-height default-font)
;;; serial-number is incremented every time a new time stamp is generated, and is
;;; only there for human beings looking at the stamps.
;;; Other elements may be added as needed.
(DEFFLAVOR SCALE-INFERIORS-MIXIN (CURRENT-TIME-STAMP) ()
  (:INCLUDED-FLAVORS SHEET)
  (:GETTABLE-INSTANCE-VARIABLES CURRENT-TIME-STAMP))

(DEFMETHOD (SCALE-INFERIORS-MIXIN :AFTER :INIT) (IGNORE)
  (SETQ CURRENT-TIME-STAMP
	(LIST 0 (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT) *DEFAULT-FONT*)))

(DEFMETHOD (SCALE-INFERIORS-MIXIN :INFERIOR-TIME-STAMP) (INF)
  INF						;Inferiors all have same time stamp
  CURRENT-TIME-STAMP)

(DEFMETHOD (SCALE-INFERIORS-MIXIN :BEFORE :INFERIOR-ACTIVATE) (INFERIOR)
  ;Catch up with any changes that happened while we were inactive
  (SCALE-INFERIORS-MIXIN-UPDATE-INFERIOR INFERIOR)
  INFERIOR)

(DEFWRAPPER (SCALE-INFERIORS-MIXIN :CHANGE-OF-SIZE-OR-MARGINS) (IGNORE . BODY)
  `(DELAYING-SCREEN-MANAGEMENT
     (LET ((OLD-EXP-INFS (REVERSE EXPOSED-INFERIORS)))
       (DOLIST (I EXPOSED-INFERIORS)
	 (FUNCALL I ':DEEXPOSE))
       ,@BODY
       (SETQ CURRENT-TIME-STAMP
	     (LIST (1+ (CAR CURRENT-TIME-STAMP))
		   (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT) *DEFAULT-FONT*))
       (DOLIST (I OLD-EXP-INFS)
	 (SCALE-INFERIORS-MIXIN-SCALE-INFERIOR I T))
       (DOLIST (I INFERIORS)
	 (OR (MEMQ I EXPOSED-INFERIORS)
	     (SCALE-INFERIORS-MIXIN-SCALE-INFERIOR I NIL))))))

(DEFMETHOD (SCALE-INFERIORS-MIXIN :BEFORE :CHANGE-OF-DEFAULT-FONT) (IGNORE NEW-FONT)
  (SETQ CURRENT-TIME-STAMP
	(LIST (1+ (CAR CURRENT-TIME-STAMP))
	      (SHEET-INSIDE-WIDTH) (SHEET-INSIDE-HEIGHT) NEW-FONT)))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (SCALE-INFERIORS-MIXIN)
(DEFUN SCALE-INFERIORS-MIXIN-UPDATE-INFERIOR (INFERIOR)
  (LET ((INF-TIME-STAMP (SHEET-TIME-STAMP INFERIOR)))
    (COND ((NEQ INF-TIME-STAMP CURRENT-TIME-STAMP)
	   (LET ((OLD-FONT (FOURTH (SHEET-TIME-STAMP INFERIOR)))
		 (NEW-FONT (FOURTH CURRENT-TIME-STAMP)))
	     (OR (EQ OLD-FONT NEW-FONT)
		 (FUNCALL INFERIOR ':CHANGE-OF-DEFAULT-FONT OLD-FONT NEW-FONT)))
	   (SCALE-INFERIORS-MIXIN-SCALE-INFERIOR INFERIOR NIL INF-TIME-STAMP))))))

(DECLARE-FLAVOR-INSTANCE-VARIABLES (SCALE-INFERIORS-MIXIN)
(DEFUN SCALE-INFERIORS-MIXIN-SCALE-INFERIOR (INFERIOR EXPOSE
				&OPTIONAL (INF-TIME-STAMP (SHEET-TIME-STAMP INFERIOR)))
  (OR (EQ CURRENT-TIME-STAMP INF-TIME-STAMP)
      ;; Hasn't had edges set in the current time slice, so set them
      (LET* ((SIZE-LAST-TIME (CDR INF-TIME-STAMP))
	     (NEW-LEFT (// (* (SHEET-X-OFFSET INFERIOR) (SHEET-INSIDE-WIDTH))
			   (FIRST SIZE-LAST-TIME)))
	     (NEW-TOP (// (* (SHEET-Y-OFFSET INFERIOR) (SHEET-INSIDE-HEIGHT))
			  (SECOND SIZE-LAST-TIME)))
	     (NEW-WIDTH (// (* (SHEET-WIDTH INFERIOR) (SHEET-INSIDE-WIDTH))
			    (FIRST SIZE-LAST-TIME)))
	     (NEW-HEIGHT (// (* (SHEET-HEIGHT INFERIOR) (SHEET-INSIDE-HEIGHT))
			     (SECOND SIZE-LAST-TIME))))
	(COND ((AND (= (SHEET-X-OFFSET INFERIOR) NEW-LEFT)
		    (= (SHEET-Y-OFFSET INFERIOR) NEW-TOP)
		    (= (SHEET-WIDTH INFERIOR) NEW-WIDTH)
		    (= (SHEET-HEIGHT INFERIOR) NEW-HEIGHT))
	       (SETQ NEW-LEFT NIL))
	      ((NOT (FUNCALL INFERIOR ':SET-EDGES NEW-LEFT NEW-TOP
			     (+ NEW-LEFT NEW-WIDTH) (+ NEW-TOP NEW-HEIGHT) ':VERIFY))
	       ;; Won't go, try not to change size
	       (SETQ NEW-WIDTH (SHEET-WIDTH INFERIOR)
		     NEW-HEIGHT (SHEET-HEIGHT INFERIOR))
	       (AND (> (+ NEW-WIDTH NEW-LEFT) (SHEET-INSIDE-RIGHT))
		    (SETQ NEW-LEFT (- (SHEET-INSIDE-RIGHT) NEW-WIDTH)))
	       (AND (> (+ NEW-HEIGHT NEW-TOP) (SHEET-INSIDE-BOTTOM))
		    (SETQ NEW-TOP (- (SHEET-INSIDE-BOTTOM) NEW-HEIGHT)))
	       (OR (FUNCALL INFERIOR ':SET-EDGES NEW-LEFT NEW-TOP
			    (+ NEW-LEFT NEW-WIDTH) (+ NEW-TOP NEW-HEIGHT) ':VERIFY)
		   ;; Won't go, don't change size at all
		   (SETQ NEW-LEFT NIL))))
	(COND (NEW-LEFT
	       (FUNCALL INFERIOR ':SET-EDGES
			NEW-LEFT NEW-TOP (+ NEW-LEFT NEW-WIDTH) (+ NEW-TOP NEW-HEIGHT))
	       (AND EXPOSE (FUNCALL INFERIOR ':EXPOSE)))
	      (T (FUNCALL INFERIOR ':UPDATE-TIME-STAMP)))))))

(DEFFLAVOR STANDARD-SCREEN () (SCALE-INFERIORS-MIXIN SCREEN))

;;; Before making our first screen, compile any methods it requires
;;; Also do Sheet now, since it actually does get instantiated, e.g. for the who-line
(COMPILE-FLAVOR-METHODS SHEET SCREEN STANDARD-SCREEN)

;;; This height may get hacked by the who-line making code if the wholine ends up
;;; at the bottom of the main screen (which it usually does!)
(DEFVAR MAIN-SCREEN-WIDTH 768.)
(DEFVAR MAIN-SCREEN-HEIGHT 896.)

;;;Set things up
(DEFUN INITIALIZE ()
  (SHEET-CLEAR-LOCKS)
  (WHO-LINE-SETUP)
  ;; Set up screen and sheet for the main monitor (CPT typically)
  (AND (NOT (BOUNDP 'MAIN-SCREEN))
       (SETQ MAIN-SCREEN
	     (DEFINE-SCREEN 'STANDARD-SCREEN "Main Screen"
			    ':BUFFER (LSH 77 18.)
			    ':CONTROL-ADDRESS 377760
			    ':PROPERTY-LIST '(:VIDEO :BLACK-AND-WHITE
					      :CONTROLLER :SIMPLE)
			    ':HEIGHT (- MAIN-SCREEN-HEIGHT (SHEET-HEIGHT WHO-LINE-SCREEN))
			    ':WIDTH MAIN-SCREEN-WIDTH)))
  (SETQ MOUSE-SHEET MAIN-SCREEN)
  (SETQ DEFAULT-SCREEN MAIN-SCREEN
	INHIBIT-SCREEN-MANAGEMENT NIL
	SCREEN-MANAGER-TOP-LEVEL T
	SCREEN-MANAGER-QUEUE NIL))

(DEFUN DEFINE-SCREEN (FLAVOR NAME &REST ARGS)
  (LET ((SCREEN (LEXPR-FUNCALL #'MAKE-WINDOW FLAVOR ':NAME NAME ARGS)))
    (PUSH SCREEN ALL-THE-SCREENS)
    (FUNCALL SCREEN ':EXPOSE)
    SCREEN))

(DEFVAR MAIN-SCREEN-AND-WHO-LINE NIL)
(DEFUN MAIN-SCREEN-AND-WHO-LINE ()
  (IF MAIN-SCREEN-AND-WHO-LINE
      (SI:CHANGE-INDIRECT-ARRAY MAIN-SCREEN-AND-WHO-LINE (SHEET-ARRAY-TYPE MAIN-SCREEN)
				(LIST MAIN-SCREEN-WIDTH MAIN-SCREEN-HEIGHT)
				(SCREEN-BUFFER MAIN-SCREEN) NIL)
      (SETQ MAIN-SCREEN-AND-WHO-LINE
	    (MAKE-ARRAY (LIST MAIN-SCREEN-WIDTH MAIN-SCREEN-HEIGHT)
			':TYPE (SHEET-ARRAY-TYPE MAIN-SCREEN)
			':DISPLACED-TO (SCREEN-BUFFER MAIN-SCREEN))))
  MAIN-SCREEN-AND-WHO-LINE)

(DEFVAR INITIAL-LISP-LISTENER)

;This function is called from an initialization in COMETH
(DEFUN WINDOW-INITIALIZE ()
  (INITIALIZE)
  (DOLIST (S ALL-THE-SCREENS)
    (FUNCALL S ':EXPOSE))
  (SETQ KBD-TYI-HOOK NIL PROCESS-IS-IN-ERROR NIL)
  (OR (EQ WHO-LINE-PROCESS SI:INITIAL-PROCESS)	;So it stays latched here during loading
      (SETQ WHO-LINE-PROCESS NIL))
  (OR INITIAL-LISP-LISTENER	;Set to NIL in LTOP
      (SETQ INITIAL-LISP-LISTENER (MAKE-WINDOW 'LISP-LISTENER ':PROCESS SI:INITIAL-PROCESS)))
  (FUNCALL INITIAL-LISP-LISTENER ':SELECT)
  (OR (MEMQ 'TV:BLINKER-CLOCK SI:CLOCK-FUNCTION-LIST)
      (PUSH 'TV:BLINKER-CLOCK SI:CLOCK-FUNCTION-LIST))
  (INSTALL-MY-KEYBOARD))

(ADD-INITIALIZATION "Clear Initial Lisp Listener"
		    '(SHEET-FORCE-ACCESS (INITIAL-LISP-LISTENER)
		       (FUNCALL INITIAL-LISP-LISTENER ':REFRESH))
		    '(BEFORE-COLD))

(DEFUN SET-TV-SPEED (FREQUENCY)
  "Set the TV refresh rate.  The default is 64.69.  Returns the number of display lines."
  ;; Try not to burn up the monitor
  (CHECK-ARG FREQUENCY (AND (> FREQUENCY 54.) (< FREQUENCY 76.))
	     "a number between 55. and 75.")
  ;; Here each horizontal line is 32. sync clocks, or 16.0 microseconds with a 64 MHz clock.
  ;; The number of lines per frame is 70. overhead lines plus enough display lines
  ;; to give the desired rate.
  (DELAYING-SCREEN-MANAGEMENT
   (WITH-MOUSE-USURPED
    (LOCK-SHEET (MAIN-SCREEN)
     (LOCK-SHEET (WHO-LINE-SCREEN)
       (WITHOUT-INTERRUPTS
	 (LET ((MS MOUSE-SHEET) (SW SELECTED-WINDOW))
	   (AND (SHEET-ME-OR-MY-KID-P MS MAIN-SCREEN)
		(SETQ MOUSE-SHEET NIL))
	   (FUNCALL WHO-LINE-SCREEN ':DEEXPOSE)
	   (FUNCALL MAIN-SCREEN ':DEEXPOSE)
	   (LET ((N-LINES (- (FIX (// 1e6 (* 16. FREQUENCY))) 70.)))
	     (SI:SETUP-CPT
	       (SETQ SYNC-RAM-CONTENTS	;save for possible use at LISP-REINITIALIZE.
		(APPEND '(1.  (1 33) (5 13) 12 12 (11. 12 12) 212 113)	;VERT SYNC, CLEAR TVMA
		       '(53. (1 33) (5 13) 12 12 (11. 12 12) 212 13)	;VERT RETRACE
		       '(8.  (1 31)  (5 11) 11 10 (11. 0 0) 200 21)	;8 LINES OF MARGIN
		       (DO ((L NIL (APPEND L `(,DN (1 31) (5 11) 11 50 (11. 0 40) 200 21)))
			    (N N-LINES (- N DN))
			    (DN))
			   ((ZEROP N) L)
			 (SETQ DN (MIN 255. N)))
		       '(7. (1 31) (5 11) 11 10 (11. 0 0) 200 21)
		       '(1. (1 31) (5 11) 11 10 (11. 0 0) 300 23)))
	       (SCREEN-CONTROL-ADDRESS MAIN-SCREEN)
	       T)
	     ;; Move the who-line, and change the dimensions of main screen
	     (SETQ MAIN-SCREEN-HEIGHT N-LINES)
	     (FUNCALL WHO-LINE-SCREEN ':CHANGE-OF-SIZE-OR-MARGINS
		      ':TOP (- MAIN-SCREEN-HEIGHT (SHEET-HEIGHT WHO-LINE-SCREEN)))
	     (FUNCALL MAIN-SCREEN ':CHANGE-OF-SIZE-OR-MARGINS
		      ':HEIGHT (- MAIN-SCREEN-HEIGHT (SHEET-HEIGHT WHO-LINE-SCREEN)))
	     (SETQ %DISK-RUN-LIGHT
		   (+ (- (* MAIN-SCREEN-HEIGHT (SHEET-LOCATIONS-PER-LINE MAIN-SCREEN)) 15)
		      (LSH 77 18.)))
	     (SETQ WHO-LINE-RUN-LIGHT-LOC (+ 2 (LOGAND %DISK-RUN-LIGHT 777777)))
	     (FUNCALL WHO-LINE-SCREEN ':EXPOSE)
	     (FUNCALL MAIN-SCREEN ':EXPOSE)
	     (AND SW (FUNCALL SW ':SELECT))
	     (MOUSE-SET-SHEET MS)
	     N-LINES))))))))

(DEFUN SET-WHO-LINE-LINES (N-LINES)
  (WITH-MOUSE-USURPED
   (LOCK-SHEET (MAIN-SCREEN)
    (LOCK-SHEET (WHO-LINE-SCREEN)
      (WITHOUT-INTERRUPTS
	(LET ((MS MOUSE-SHEET) (SW SELECTED-WINDOW))
	  (AND (SHEET-ME-OR-MY-KID-P MS MAIN-SCREEN)
	       (SETQ MOUSE-SHEET NIL))
	  (FUNCALL WHO-LINE-SCREEN ':DEEXPOSE)
	  (FUNCALL MAIN-SCREEN ':DEEXPOSE)
;	  (FUNCALL WHO-LINE-SCREEN ':SET-VSP (IF (= N-LINES 1) 0 2))
	  (FUNCALL WHO-LINE-SCREEN ':CHANGE-OF-SIZE-OR-MARGINS
		   ':BOTTOM MAIN-SCREEN-HEIGHT 
		   ':TOP (- MAIN-SCREEN-HEIGHT
			    (* N-LINES (SHEET-LINE-HEIGHT WHO-LINE-SCREEN))))
	  (FUNCALL MAIN-SCREEN ':CHANGE-OF-SIZE-OR-MARGINS
		   ':HEIGHT (- MAIN-SCREEN-HEIGHT (SHEET-HEIGHT WHO-LINE-SCREEN)))
	  (FUNCALL WHO-LINE-SCREEN ':EXPOSE)
	  (FUNCALL MAIN-SCREEN ':EXPOSE)
	  (AND SW (FUNCALL SW ':SELECT))
	  (SETQ MOUSE-SHEET MS)))))))

(DEFUN SET-DEFAULT-FONT (NEW-FONT)
  ;; Blow out here if font is illegal
  (SETQ NEW-FONT (FUNCALL MAIN-SCREEN ':PARSE-FONT-DESCRIPTOR NEW-FONT))
  ;; Now change everyone who cares to be changed
  (LET ((OLD-FONT *DEFAULT-FONT*))
    (SETQ *DEFAULT-FONT* NEW-FONT)
    (DOLIST (SCREEN ALL-THE-SCREENS)
      (FUNCALL SCREEN ':CHANGE-OF-DEFAULT-FONT
		      (FUNCALL SCREEN ':PARSE-FONT-DESCRIPTOR OLD-FONT)
		      (FUNCALL SCREEN ':PARSE-FONT-DESCRIPTOR NEW-FONT))))
  NEW-FONT)
