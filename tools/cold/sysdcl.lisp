;;;-*- Mode:LISP; Package:SYSTEM-INTERNALS -*-
;;; Declarations for SYSTEM's initally loaded
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;; Cut down version for Common Lisp
;;
;; Only contains the cold load files.

(in-package :cold)

;;; These are the files in the cold load
(defparameter +cold-load-file-list+
	  '("sys:fonts;cptfont.qfasl"
	    "sys:sys;qrand.qfasl"
	    "sys:io;qio.qfasl"
;	    "sys:io;rdtbl.qfasl"	;done specially
	    "sys:io;read.qfasl"
	    "sys:io;print.qfasl"
	    "sys:window;cold.qfasl"
	    "sys:io;debug.qfasl"
	    "sys:sys;sgfctn.qfasl"
	    "sys:sys;qev.qfasl"
	    "sys:sys;ltop.qfasl"
	    "sys:sys;qfasl.qfasl"
;	    "sys:io;mini.qfasl"     
	    "sys:network;tftp-mini.qfasl"
	    "sys:sys;qfctns.qfasl"
	    "sys:sys2;string.qfasl"
	    ))

;;; These variables are looked at by the cold load generator, who takes
;;; the translated pathnames and dumps out prototype values into the new
;;; world with those strings suitable for use with MINI.
;;; They are then used before this file gets loaded.
(defparameter mini-file-alist-list
	  '(load-packages-file-alist-1 load-packages-file-alist-2
				       global-package-file-alist
	    inner-system-file-alist
	    chaos-file-alist
	    site-file-alist host-table-file-alist))

(defparameter load-packages-file-alist-1
	  '(("sys:sys;pack4.qfasl" "")))

(defparameter load-packages-file-alist-2
	  '(("sys:sys;pkgdcl.lisp" "")
	    ))

(defparameter global-package-file-alist
	  '(("sys:cold;global.lisp" "GLOBAL")
	    ("sys:cold;system.lisp" "SYSTEM")))

(defparameter inner-system-file-alist
	  '(("sys:sys;qmisc.qfasl" "SI")
	    ("sys:sys;sort.qfasl" "SI")		;Needed by FLAVOR
	    ("sys:sys2;defsel.qfasl" "SI")	;Needed by FQUERY
	    ("sys:io;format.qfasl" "FORMAT")	;ditto
	    ("sys:io1;fquery.qfasl" "FORMAT")	;Needed by everything in sight
	    ("sys:sys2;flavor.qfasl" "SI")	;Needed by PROCES
	    ("sys:sys2;prodef.qfasl" "SI")	;Definitions for PROCES
	    ("sys:sys2;proces.qfasl" "SI")
	    ("sys:window;eh.qfasl" "EH")
	    ("sys:window;ehr.qfasl" "EH")
	    ("sys:window;ehc.qfasl" "EH")
	    ("sys:sys2;disass.qfasl" "COMPILER")	;EH calls subroutines in DISASS
	    ("sys:io;disk.qfasl" "SI")
	    ("sys:sys2;login.qfasl" "SI")	;ditto
	    ("sys:io;rddefs.qfasl" "SI")	;Load this before trying to read any #\'s
	    ("sys:sys2;host.qfasl" "SI")
	    ("sys:sys2;hash.qfasl" "SI")	;Needed by PATHNM
	    ("sys:io;stream.qfasl" "SI")	;Probably needed by any file system
	    ;; PATHNM must be the last file in this list.  It breaks things while cold loading
	    ;; that QLD knows how to fix after this alist is loaded.
	    ("sys:io;pathnm.qfasl" "FS")
	    ))

(defparameter chaos-file-alist
	  '(("sys:io;chsncp.qfasl" "CHAOS")
	    ("sys:io;chsaux.qfasl" "CHAOS")
	    ("sys:io;qfile.qfasl" "FS")
	    ))

(defparameter site-file-alist
	  '(("sys:site;site.qfasl" "SI")
	    ))

(defparameter host-table-file-alist
	  '(
	    ("sys:site;hsttbl.qfasl" "CHAOS")
	    ("sys:site;lmlocs.qfasl" "SI")
	    ))
