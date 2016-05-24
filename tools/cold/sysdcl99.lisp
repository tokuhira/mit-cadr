;;;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10 -*-

;;; Declarations for SYSTEM's initally loaded
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(in-package :cold)

;;;; These are the files in the cold load
(defparameter +cold-load-file-list+
	  '("sys:fonts;cptfon.qfasl"
	    "sys:sys;qrand.qfasl"
;	    "sys:sys;fspec.qfasl"
	    "sys:io;qio.qfasl"
;	    "sys:io;rdtbl.qfasl"	;done specially
;	    "sys:io;crdtbl.qfasl"	;done specially
	    "sys:io;read.qfasl"
	    "sys:io;print.qfasl"
	    "sys:window;cold.qfasl"
	    "sys:sys;sgfctn.qfasl"
	    "sys:sys;eval.qfasl"
	    "sys:sys;types.qfasl"
	    "sys:sys;ltop.qfasl"
	    "sys:sys;qfasl.qfasl"
;	    "sys:io;mini.qfasl"
	    "sys:network;tftp-mini.qfasl"
	    "sys:sys;qfctns.qfasl"
	    "sys:sys2;string.qfasl"
	    "sys:sys2;character.qfasl"
	    "sys:sys;clpack.qfasl"
	    "sys:cold;global.qfasl"
	    "sys:cold;system.qfasl"
	    "sys:cold;lisp.qfasl"))

;;; These variables are looked at by the cold load generator, which takes
;;; the translated pathnames and dumps out prototype values into the new
;;; world with those strings suitable for use with MINI.
;;; They are then used before this file gets loaded.
(defparameter mini-file-alist-list
	  '(inner-system-file-alist rest-of-pathnames-file-alist
	    ethernet-file-alist site-file-alist host-table-file-alist))

(defparameter inner-system-file-alist
	  '(("sys:sys2;defsel.qfasl" "SI")	;By (resource named-structure-invoke)
	    ("sys:sys2;resour.qfasl" "SI")	;By FILLARRAY
	    ("sys:sys;qmisc.qfasl" "SI")
	    ("sys:sys;sort.qfasl" "SI")		;Needed by FLAVOR
	    ("sys:io;format.qfasl" "FORMAT")	;ditto
	    ("sys:io1;fquery.qfasl" "FORMAT")	;Needed by everything in sight
	    ("sys:sys2;hash.qfasl" "SI")	;Needed by FLAVOR,PATHNM
	    ("sys:sys2;flavor.qfasl" "SI")	;Needed by PROCES
	    ("sys:sys2;hashfl.qfasl" "SI")	;Make flavors really work.
	    ("sys:sys2;prodef.qfasl" "SI")	;Definitions for PROCES
	    ("sys:sys2;proces.qfasl" "SI")
	    ("sys:sys2;numer.qfasl" "SI")	;SI:EXPT-HARD needed by PROCES
	    ("sys:eh;eh.qfasl" "EH")
	    ("sys:eh;ehf.qfasl" "EH")
	    ("sys:eh;ehc.qfasl" "EH")
	    ("sys:eh;ehbpt.qfasl" "EH")
	    ("sys:sys2;disass.qfasl" "COMPILER")	;EH calls subroutines in DISASS
	    ("sys:sys2;describe.qfasl" "SI")	;For hack value
	    ("sys:io;disk.qfasl" "SI")
	    ("sys:sys2;login.qfasl" "SI")	;ditto
	    ("sys:io;rddefs.qfasl" "SI")	;Load this before trying to read any #\'s
	    ("sys:network;host.qfasl" "SI")
	    ("sys:network;package.qfasl" "SI")
	    ("sys:io;file;access.qfasl" "FS")
	    ("sys:io;stream.qfasl" "SI")	;Probably needed by any file system
	    ;; PATHNM must be the last file in this list.  It breaks things while cold loading
	    ;; that QLD knows how to fix after this alist is loaded.
	    ("sys:io;file;pathnm.qfasl" "FS")))

(defparameter rest-of-pathnames-file-alist
	  '(("sys:io;file;pathst.qfasl" "FS")
	    ("sys:io;file;logical.qfasl" "FS")
	    ("sys:file2;pathnm.qfasl" "FS")
	    ("sys:file;lmpars.qfasl" "FS")
	    ("sys:io;file;open.qfasl" "FS")
	    ("sys:network;chaos;chsncp.qfasl" "CHAOS")
	    ("sys:network;chaos;chuse.qfasl" "CHAOS")
	    ("sys:network;chaos;qfile.qfasl" "FS")))

(defparameter ethernet-file-alist
	  '(("sys:io;simple-ether.qfasl" "ETHERNET")
	    ("sys:io;addr-res.qfasl" "ETHERNET")))

(defparameter site-file-alist
	  '(("sys:site;site.qfasl" "SI")))

(defparameter host-table-file-alist
	  '(("sys:site;hsttbl.qfasl" "CHAOS")
	    ("sys:site;lmlocs.qfasl" "SI")))
