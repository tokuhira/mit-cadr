;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for ZMail version 38.2
;;; Reason: Fix (MOUSE-SENSITIVE-MODE-LINE-WINDOW :MOUSE-MOVES) for 78.13
;;; Written 12/15/81 16:01:53 by MMcM,
;;; while running on Lisp Machine Five from band 2
;;; with System 78.16, ZMail 38.1, microcode 836.

; From file WINDOW > ZMAIL; AI:
#8R ZWEI:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "ZWEI")))

(DEFMETHOD (MOUSE-SENSITIVE-MODE-LINE-WINDOW :MOUSE-MOVES) (X Y &AUX STRING X1
								(X0 (TV:SHEET-INSIDE-LEFT)))
  (TV:MOUSE-SET-BLINKER-CURSORPOS)
  (COND ((AND ( Y (TV:SHEET-INSIDE-TOP))
	      (< Y TV:(+ (SHEET-INSIDE-TOP) LINE-HEIGHT))
	      (SETQ STRING (DOLIST (STRING PREVIOUS-MODE-LINE)
			     (IF (AND (LISTP STRING) (EQ (CAR STRING) ':RIGHT-FLUSH))
				 (SETQ X1 (TV:SHEET-INSIDE-RIGHT)
				       STRING (CADR STRING))
			         (SETQ X1 (+ X0 (TV:SHEET-STRING-LENGTH SELF STRING))))
			     (AND (> X1 X) (RETURN STRING))
			     (SETQ X0 X1)))
	      (DOLIST (ELEMENT (CAR SELECTABLE-ELEMENTS-LOCATION))
		(AND (EQ STRING (SYMEVAL-IN-INSTANCE TV:SUPERIOR (CAR ELEMENT)))
		     (RETURN (SETQ CURRENT-ITEM (CDR ELEMENT))))))
	 (TV:BLINKER-SET-CURSORPOS ITEM-BLINKER (- X0 (TV:SHEET-INSIDE-LEFT)) 0)
	 (TV:BLINKER-SET-SIZE ITEM-BLINKER (- X1 X0) (FONT-BLINKER-HEIGHT TV:CURRENT-FONT))
	 (TV:BLINKER-SET-VISIBILITY ITEM-BLINKER T))
	(T
	 (TV:BLINKER-SET-VISIBILITY ITEM-BLINKER NIL)
	 (SETQ CURRENT-ITEM NIL))))

)

