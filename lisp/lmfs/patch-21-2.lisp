;;; -*- Mode: Lisp; Package: User; Base: 8.; Patch-File: T -*-
;;; Patch file for LMFS version 21.2
;;; Reason: Backup: goes to end of extant tape when appropriate.
;;; Written 12/15/81 12:14:32 by BSG,
;;; while running on Pointer from band 1
;;; with System 78.14, ZMail 38.1, Experimental Symbolics 8.3, Experimental Tape 6.0, Experimental LMFS 21.1, Experimental Canon 9.0, microcode 840.



; From file BACKUP.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun backup-forward-to-end ()
  (funcall *backup-stream* ':rewind)
  (do () (())
    (funcall *backup-stream* ':skip-file)
    (or (funcall *backup-stream* ':tyi)
	(return nil)))
  (funcall *backup-stream* ':backspace)
  (setq *backup-dump-bot-p* nil))

)

; From file BACKUP.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun backup-make-statements-about-the-mounted-tape (&aux dont-rewind reel
						      (unit (funcall *backup-stream* ':unit)))
  (setq *backup-dump-bot-p* t)
  (with-open-stream
    (*backup-stream* (tape:open-tape unit ':mode ':read ':no-bot-prompt t
				     ':no-read-ahead t))
    (*catch 'reloader-no-prelude
      (do () (())
	(cond ((string-equal "PRELUDE"
			     (condition-bind
			       ((:tape-error
				  #'(lambda (&rest ignore)
				      (format t "~&Tape error checking tape - We ~
				     conclude that this is a fresh tape.")
				      (*throw 'reloader-no-prelude nil))))
			       (funcall *backup-stream* ':line-in)) 0 0 nil 7)
	       (funcall *backup-stream* ':rewind)
	       (let ((prelude (reloader-read-prelude)))
		 (cond ((null (setq reel (get prelude ':reel)))
			(return nil)) ;Not even a backup tape
		       ((string-equal (string-trim " " reel) *backup-dump-tape-name*)
			;; Equal case, this reel matches
			(cond ((yes-or-no-p
				 (format nil "~&Do you wish to overwrite tape ~A?~@
					 The usual answer here is /"no/", meaning append to~@
					 the end of tape ~A.  If you answer /"yes/", tape ~A~@
					 will be irretrievably overwritten: "
					 reel reel reel))
			       (setq *backup-dump-bot-p* t)
			       (funcall *backup-stream* ':rewind))
			      (t  (backup-forward-to-end)
				  (setq *backup-dump-bot-p* nil dont-rewind t)))
			(return))
		       ;;Some other backup tape
		       ((yes-or-no-p		;Wants to use mounted one instead
			  (format nil "~&Tape ~A, containing a ~A~@
			taken by ~A at ~A appears to be on drive ~D.~@
			Did you really want to use reel ~A instead? "
				  reel
				  (or (get prelude ':dump-name)
				      (get prelude ':type))
				  (get prelude ':user-id)
				  (if (get prelude ':dump-time)
				      (time:print-universal-time
					(get prelude ':dump-time) nil)
				    "???")
				  (funcall *backup-stream* ':unit)
				  reel))
			       (funcall *backup-stream* ':rewind)
			       (setq *backup-dump-tape-name* reel
				     *backup-dump-bot-p* t
				     *backup-tape-name-8* (format
							    nil "~8a"
							    *backup-dump-tape-name*))
			       ;;Loop thru once more...
			       )
		       ((yes-or-no-p		;Overwrite this reel
			  (format
			    nil "~&In that case, do you want to OVERWRITE~@
				the reel labelled ~A, and make it be reel ~A?~@
				(an answer of /"no/" means you want to mount another one): "
			    reel *backup-dump-tape-name*))
			(funcall *backup-stream* ':rewind)
			(setq *backup-dump-bot-p* t)
			(return))
		       (t (rewind-unload-new-tape)))))
	      (t (return))))))
  (or dont-rewind (funcall *backup-stream* ':rewind)))

)

; From file BACKUP.LISP DSK:<LMFS> SCRC:
#8R LMFS:(COMPILER-LET ((PACKAGE (PKG-FIND-PACKAGE "LMFS")))

(defun create-backup-dump-history (dumpid)
  (let ((ht (make-equal-hash-table))
	(*cbdh-ht-addr* 6)			;never gets to be zero, allows zero sentinels
	(*cbdh-strings*  (list "DUMY")))		;so that ht works, reader will skip.
    (setq *backup-dump-history* (reverse *backup-dump-history*))	;let fcn restart,
						;nreverse is non-obvious screw to user
    (setq dumpid (cbdh-hash ht dumpid))
    (let ((file-image
	    (loop for (type object creation-date) in *backup-dump-history*
		  collect
		  (selectq type
		    (:file       (list (cbdh-hash ht (funcall object ':name))
				       (cbdh-hash ht (funcall object ':type))
				       (funcall object ':version)
				       creation-date))
		    (:directory  (list (cbdh-hash ht (funcall object ':string-for-host))
				       0 0 0))))))
      (let ((path (funcall (dump-directory-template)
			   ':new-name *backup-dump-tape-name*))
	    (newp t)
	    (adrbas 0))
	(if (and (probef path)
		 (not *backup-dump-bot-p*))
	    (setq newp nil))
	(with-open-file (file path
			      ':direction (if newp ':output ':append)
			      ':characters nil ':byte-size 8.)
	  (if (not newp)
	      (setq adrbas (funcall file ':read-pointer)))
	  (format t "~&~:[Appending to~;Creating~] ~A" newp  (funcall file ':truename))
	  (dolist (s (nreverse *cbdh-strings*))
	    (cbdh-put-fix file (string-length s) 2)
	    (funcall file ':                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                