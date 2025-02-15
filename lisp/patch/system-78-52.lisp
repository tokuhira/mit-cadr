;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for System version 78.52
;;; Reason: Add SEND, GETF, WHEN and UNLESS.
;;; Written 6/09/10 23:44:09 by RJS,
;;; while running on Unknown from band 1
;;; with System 78.51, ZMail 38.5, Tape 6.5, LMFS 21.34, Symbolics 8.13, microcode 841.



; From file lmmac.lisp >sys2 UNKNOWN:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(DEFMACRO GETF (PLACE PROPERTY &OPTIONAL (DEFAULT NIL))
  `(OR (GET (LOCF ,PLACE) ,PROPERTY) ,DEFAULT))

(GLOBALIZE 'GETF)

)

; From file lmmac.lisp >sys2 UNKNOWN:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(DEFMACRO SEND (OBJECT OPERATION &REST ARGUMENTS)
  `(FUNCALL ,OBJECT ,OPERATION . ,ARGUMENTS))

;;;---!!! Can't do GLOBALIZE on SEND due to:
;;;---!!!    >>ERROR: Multiple function definitions for SI:SEND, in SYSTEM-INTERNALS and LMFS

)

; From file lmmac.lisp >sys2 UNKNOWN:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;;; (WHEN pred {form}*)
(DEFMACRO WHEN (PRED &BODY BODY)
  `(AND ,PRED (PROGN ,@BODY)))

(GLOBALIZE 'WHEN)

)

; From file lmmac.lisp >sys2 UNKNOWN:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

;;; (UNLESS pred {form}*)
(DEFMACRO UNLESS (PRED &BODY BODY)
  `(IF ,PRED () ,@BODY))

(GLOBALIZE 'UNLESS)

)

; From file lmmac.lisp >sys2 UNKNOWN:
#8R SYSTEM-INTERNALS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))

(DEFMACRO DO-FOREVER (&BODY BODY)
  `(DO ()
       (())
     . ,BODY))

(GLOBALIZE 'DO-FOREVER)

)
