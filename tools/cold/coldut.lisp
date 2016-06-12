; -*- Mode:LISP; Package:COLD; Base:8; Lowercase:T; Readtable:T -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

; Utilities for cold-load generator

(in-package :cold)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To compile this:			       ;;;
;;;   (1) Load the old QFASL of it	       ;;;
;;;   (2) Run (LOAD-PARAMETERS)		       ;;;
;;;   (3) Now you may compile it	       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Little variables that have to do with the word format
(defvar big-fixnum)
(defvar little-fixnum)
(defvar q-typed-pointer-mask)	;Due to deficiencies in LDB and DPB
(defvar q-pointer-mask)
(defvar array-index-order nil)

;; Needed for sys99
(defvar sym:lambda-list-keywords
              '(&optional &rest &key &allow-other-keys &aux
                &special &local &functional &eval &quote
                &environment &list-of &body &whole))

(defun assign-alternate (x)
   (prog nil 
    l	(cond ((null x)(return nil)))
	(set (intern (symbol-name (car x))) (cadr x))
	(setq x (cddr x))
	(go l)))

(defun get-alternate (x)
   (prog (y)
    l	(cond ((null x) (return (reverse y))))
	(setq y (cons (car x) y))
	(setq x (cddr x))
	(go l)))

(defun assign-values (input-list &optional (shift 0) (init 0) (delta 1))
   (prog ()
lp	(cond ((null input-list) (return init)))
	(proclaim `(special ,(car input-list)))
	(set (car input-list) (ash init shift))
	(setq input-list (cdr input-list))
	(setq init (+ init delta))
	(go lp)))

(defun assign-values-init-delta (input-list shift init delta)
    (prog nil 
lp	(cond ((null input-list) (return init)))
	(set (car input-list) (ash init shift))
	(setq input-list (cdr input-list))
	(setq init (+ init delta))
	(go lp)))

(defmacro defprop (s v p)
  `(setf (get ',s ',p) ',v))

(defun atomeval (x)
	(cond ((numberp x) x)
	      (t (symbol-value x))))

(defun list-product (x)
  (do ((l x (cdr l))
       (ans 1))
      ((null l) ans)
    (setq ans (* ans (atomeval (car l))))))
       
(defun memq (o l)
  (member o l :test #'eq))

(defun lispm-ldb (ppss val)
  (let ((p (ldb (byte 5 6) ppss))
	(s (ldb (byte 5 0) ppss)))
    (ldb (byte s p) val)))

(defun lispm-dpb (newbyte ppss val)
  (let ((p (ldb (byte 5 6) ppss))
	(s (ldb (byte 5 0) ppss)))
    (dpb newbyte (byte s p) val)))

(defun bit-test (x y)
  (not (zerop (logand x y))))

(defun cold-pathname (p)
  (format nil "~A: ~{/~A~}/~A.~A"
	  (car p) (caddr p) (cadddr p) (car (cddddr p))))

(defun %make-pointer (data-type address)
  (lispm-dpb data-type sym:%%q-all-but-pointer address))

(defun %pointer (x)
  (logand q-pointer-mask x))

(defun lispm-length (x)
  (cond ((atom x) 0)
	((and (consp x) (atom (cdr x))) 1)
	(t (1+ (lispm-length (cdr x))))))

;;; The virtual memory

;; On the Lispm there is a cache of 16 pages in use and pages are read
;; in and flushed to disk as necessary. In a 64-bit Common Lisp we may
;; as well just keep all the pages in memory until the end.
;;
;; Also just use a fixnum array for each page in Common Lisp

(defvar n-vmem-pages 0)

;(i,0) is virtual page number, (i,1) is rqb
;Both slots are nil if unused
(defvar vmem-pages)

(defvar vmem-highest-address nil)

(defun vmem-initialize ()
  (setq n-vmem-pages (/ vmem-highest-address sym:page-size))
  (setq vmem-pages (make-array n-vmem-pages :initial-element nil)))

(defun write-page (p s)
  (do ((i 0 (1+ i))
       (w))
      ((= i sym:page-size) nil)
      (setq w (aref p i))
      (write-byte (ldb (byte 8 0) w) s)
      (write-byte (ldb (byte 8 8) w) s)
      (write-byte (ldb (byte 8 16) w) s)
      (write-byte (ldb (byte 8 24) w) s)))

(defun write-blank-page (s vpn)
  (do ((i 0 (1+ i))
       (w (lispm-dpb sym:dtp-free sym:%%q-data-type (* vpn sym:page-size))
	  (1+ w)))
      ((= i sym:page-size) nil)
      (write-byte (ldb (byte 8 0) w) s)
      (write-byte (ldb (byte 8 8) w) s)
      (write-byte (ldb (byte 8 16) w) s)
      (write-byte (ldb (byte 8 24) w) s)))

;Write out all the buffered pages and return the rqb's
(defun vmem-finish (name &aux buf)
  (with-open-file (s name :direction :output :if-exists :supersede
		     :element-type '(unsigned-byte 8))
    (dotimes (i n-vmem-pages)
	     (cond ((setq buf (aref vmem-pages i))
		    (write-page (aref vmem-pages i) s)
		    ;(setf (aref vmem-pages i) nil)
		    )
		   (t (write-blank-page s i))
		   ))))

;Given address returns fixnum array containing that page.
(defun vmem-find-page (address &optional (init nil))
  (if (> (logand q-pointer-mask address) vmem-highest-address)
      (error "vmem-highest-address exceeded"))
  (let* ((vpn (truncate ;(lispm-ldb sym:%%q-pointer address)
	              (logand q-pointer-mask address)
		      sym:page-size))
	 (buf (aref vmem-pages vpn)))
    (when (null buf)
      (setq buf (make-array sym:page-size :element-type 'fixnum))
      (when init
	(do ((j 0 (1+ j))
	     (tem (lispm-dpb sym:dtp-free sym:%%q-data-type
			     (* vpn sym:page-size))))
	    ((= j sym:page-size))
	    (setf (aref buf j)
		  (+ tem j))))
      (setf (aref vmem-pages vpn) buf))
    buf))
      
(defun vread (address)
  (let ((buf (vmem-find-page address)))
    (aref buf (rem address sym:page-size))))

(defun vwrite (address value)
  (let ((buf (vmem-find-page address)))
    (setf (aref buf (rem address sym:page-size)) value)))

;(defun vwrite-low (address value)
;  (let ((buf (vmem-find-page address))
;	(i (* 2 (rem address sym:page-size))))
;    (setf (aref buf i) value )))

;(defun vwrite-high (address value)
;  (let ((buf (vmem-find-page address))
;	(i (* 2 (rem address sym:page-size))))
;    (aset value buf (1+ i))))

(defun vcontents (address)
  (logand q-typed-pointer-mask (vread address)))

(defun vcdr-code (address)
  (lispm-ldb sym:%%q-cdr-code (vread address)))

(defun vstore-contents (address value)
  (let ((buf (vmem-find-page address))
	(i (rem address sym:page-size)))
    (setf (aref buf i)
	  (lispm-dpb value sym:%%q-typed-pointer (aref buf i)))))

(defun vstore-cdr-code (address value)
  (let ((buf (vmem-find-page address))
	(i (rem address sym:page-size)))
    (setf (aref buf i)
	  (lispm-dpb value sym:%%q-cdr-code (aref buf i)))))

(defun vwrite-cdr (address cdr-code value)
  (vwrite address (lispm-dpb cdr-code sym:%%q-cdr-code value)))

(defmacro vmake-pointer (data-type address)
  `(lispm-dpb ,data-type sym:%%q-all-but-pointer ,address))

(defmacro vpointer (value)
  `(logand q-pointer-mask ,value))

(defmacro vdata-type (value)
  `(lispm-ldb sym:%%q-data-type ,value))

(defmacro vfix (value)
  `(vmake-pointer sym:dtp-fix ,value))

(defun vlist (area &rest elements)
  (if (null elements)
      qnil
    (let ((value (vmake-pointer sym:dtp-list
				(store-cdr-q area sym:cdr-next (car elements)))))
      (dolist (element (cdr elements))
	(store-cdr-q area sym:cdr-next element))
      (vstore-cdr-code (+ value (length elements) -1) sym:cdr-nil)
      value)))

(defun vlist* (area &rest elements)
  (cond ((null elements) (error "Too few arguments to VLIST*"))
	((null (cdr elements)) (car elements))
	(t
	 (let ((value (vmake-pointer sym:dtp-list
				     (store-cdr-q area sym:cdr-next (car elements)))))
	   (dolist (element (cdr elements))
	     (store-cdr-q area sym:cdr-next element))
	   (vstore-cdr-code (+ value (length elements) -1) sym:cdr-error)
	   (vstore-cdr-code (+ value (length elements) -2) sym:cdr-normal)
	   value))))

(defun vcar (location)
  (vcontents location))

(defun vcdr (location)
  (let ((cdr-code (vcdr-code location)))
    (cond ((= cdr-code sym:cdr-nil) qnil)
	  ((= cdr-code sym:cdr-next) (1+ location))
	  ((= cdr-code sym:cdr-normal) (vcontents (1+ location)))
	  ((= cdr-code sym:cdr-error)
	   (error "Location ~O contains CDR-ERROR." (vpointer location))))))

;If no property, returns a NIL in this machine.
;If property found, returns other-machine pointer to cell whose car is the property value.
(defun vget-location-or-nil (location property)
  (do ((cell (vcontents location) (vcdr (vcdr cell))))
      ((= cell qnil) qnil)
    (if (= (vcontents cell) property)
	(return (vcdr cell)))))


;;;; a bit of stuff for debugging

(defun vprint-q (q)
  (format t "~%CDR-CODE ~s, DATA-TYPE ~s (~s), POINTER ~s"
	  (lispm-ldb sym:%%q-cdr-code q)
	  (vdata-type q)
	  (nth (vdata-type q) sym:q-data-types)
	  (vpointer q)))

(defvar vprinlength #o200)
(defvar vprinlevel  #o20)
(defvar vmax-stringlength #o200)

(defun vprint (typed-pointer &optional (vprinlevel vprinlevel))
  (let ((prinlength-count 0)
	(data-type (vdata-type typed-pointer))
	(pointer (vpointer typed-pointer)))
    (cond ((vatom? typed-pointer)
	   (cond ((= data-type sym:dtp-symbol)
		  (vprint-string (vcontents pointer)))
		 ((= data-type sym:dtp-fix)
		  (print pointer))
		 (t (vprint-bomb typed-pointer))))
	  ((= data-type sym:dtp-array-pointer)
	   (let ((header (vcontents pointer)))
	     (cond ((= (mask-field-from-fixnum sym:%%array-type-field header)
		       sym:art-string)
		    (princ "#\"")
		    (vprint-string typed-pointer)
		    (princ "#\""))
		   (t (vprint-bomb typed-pointer)))))
	  ((= data-type sym:dtp-list)
	   (cond ((= vprinlevel 0)
		  (princ "#"))
		 (t
		  (princ "(")
		  (prog ((l typed-pointer))
		    l	(cond ((> (setq prinlength-count (1+ prinlength-count))
				  vprinlength)
			       (princ "...")
			       (return nil))
			      ((vatom? l)
			       (cond ((vnull? l)
				      (princ ")"))
				     (t
				      (princ " . ")
				      (vprint l (1- vprinlevel))))))
		       (vprint (vcar l) (1- vprinlevel))
		       (setq l (vcdr l))
		       (go l)))))
	  (t (vprint-bomb typed-pointer)))))

(defun vprint-bomb (typed-pointer)
  (vprint-q typed-pointer))

(defun vprint-string (string)
  (let* ((pointer (vpointer string))
	 (header (vcontents pointer))
	 (long-flag (lispm-ldb sym:%%array-long-length-flag header))
	 (len (min vmax-stringlength
		   (if (zerop long-flag)
		       (lispm-ldb sym:%%array-index-length-if-short header)
		     (vpointer (1+ (vcontents pointer)))))))
    (dotimes (c len)
      (let ((wd (vread (+ pointer 1 long-flag (ash c -2)))))
	(write-char (code-char (logand #o377 (ash wd (- 0 (* 8 (logand c 3)))))))))))

(defun vatom? (typed-pointer)
  (let ((data-type (vdata-type typed-pointer)))
    (cond ((or (= data-type sym:dtp-symbol)
	       (= data-type sym:dtp-fix)
	       (= data-type sym:dtp-extended-number))
	   t))))

(defun vnull? (typed-pointer)
  (= typed-pointer qnil))

(defun mask-field-from-fixnum (ppss word)
   (logand word (lispm-dpb -1 ppss 0)))

(defvar sym-package (find-package "COLD-SYMBOLS"))
(defvar misc-function-list)
(defvar misc-instruction-list)

;;; These have to be explicitly declared special because they only exist in
;;; the cold-load generator, and are not sent over.
(declaim (special sym:cold-load-area-sizes sym:cold-load-region-sizes
		    sym:scratch-pad-pointers sym:scratch-pad-parameters
		    sym:scratch-pad-parameter-offset sym:q-corresponding-variable-lists
		    sym:support-vector-contents sym:constants-page
		    sym:read-only-area-list sym:wired-area-list sym:pdl-buffer-area-list
		    sym:list-structured-areas sym:static-areas
		    sym:a-memory-array-locations sym:new-array-index-order
		    sym:prin1 sym:base sym:ibase sym:*nopoint sym:for-cadr
		    sym:*print-base* sym:*read-base* sym:*print-radix*
		    sym:lambda-list-keywords))

;;; Set up the sym: package by loading the appropriate files
(defun load-parameters ()
  (let ((*package* sym-package)
	(*read-base* 8))
    (load "qcom.lisp")
    (load "qdefs.lisp")
    (setq misc-function-list nil)
    (setq misc-instruction-list nil)
    (load "defmic.lisp")
    (dolist (l sym:system-constant-lists)	;Make declarations so can compile self
	    (dolist (s (symbol-value l))
		    (setf (get s 'special) t)))
    (setq big-fixnum (1- (ash 1 (1- sym:%%q-pointer)))
	  little-fixnum (1- (- big-fixnum))
	  q-typed-pointer-mask (1- (ash 1 sym:%%q-typed-pointer))
	  q-pointer-mask (1- (ash 1 sym:%%q-pointer)))
    (setq sym:lambda-list-keywords
	  '(sym:&optional sym:&rest sym:&aux sym:&special sym:&local
			  sym:&functional sym:&eval sym:&quote
			  sym:&quote-dontcare sym:&dt-dontcare sym:&dt-number
			  sym:&dt-fixnum sym:&dt-symbol sym:&dt-atom sym:&dt-list
			  sym:&dt-frame sym:&function-cell sym:&list-of
			  sym:&body sym:&key sym:&allow-other-keys))))
    

;Put on QLVAL and QINTCMP properties
;Creates MISC-FUNCTION-LIST for STORE-MISC-LINK  (CALLED FROM STORE-MISC-U-ENTRY-LINKS)
; and MISC-INSTRUCTION-LIST for STORE-MICRO-CODE-SYMBOL-NAMES
(defmacro defmic (name opcode arglist lisp-function-p &optional no-qintcmp)
  (let ((function-name (if (atom name) name (car name)))
	(instruction-name (if (atom name) name (cdr name))))
    `(progn
       (cond ((not ,no-qintcmp)
	      (loop for x in ',arglist
		    when (memq x sym:lambda-list-keywords)
		    do (error "~S has ~S in its arglist which is not allowed"
			      ',instruction-name x))
	      (setf (get ',instruction-name 'sym:qintcmp) (length ',arglist))
	      (or (eq ',function-name ',instruction-name)
		  (setf (get ',function-name 'sym:qintcmp) (length ',arglist))))
	     (t				;The number of arguments is needed anyway for the cold-load generator
	      (let ((nargs (length ',arglist))
		    (restarg (memq 'sym:&rest ',arglist)))
		(loop for x in ',arglist
		      when (memq x sym:lambda-list-keywords)
		      when (not (eq x 'sym:&rest)) ;&rest allowed if no-qintcmp
		      do (error "~S has ~S in its arglist which is not allowed"
				',instruction-name x))
		;; Note that if it says &rest, for a microcode function we don't really
		;; want to get a list of args, we want to see the args on the stack, so
		;; we translate this into the maximum possible number of optional arguments.
		;; EVAL doesn't check the rest-arg bits for microcode entries anyway.
		(cond (restarg
		       (or (not ,lisp-function-p)
			   (= (length restarg) 2)
			   (error "~S has garbage ~S in its arglist" ',instruction-name restarg))
		       (setq nargs (cons (- nargs 2) #o77)))) ;(min . max)
		(setf (get ',instruction-name 'defmic-nargs-info) nargs)
		(or (eq ',function-name ',instruction-name)
		    (setf (get ',function-name 'defmic-nargs-info) nargs)))))
       (setf (get ',instruction-name 'sym:qlval) ,opcode)
       (push ',instruction-name misc-instruction-list)
       (and ,lisp-function-p
	    (push ',name misc-function-list)))))

;;;; Basic area-processing and data-storing stuff

;;; Note that area names are always symbols in the sym: package

(defvar symbol-creation-trace-list nil)
(defvar qnil)
(defvar qtruth)
(defvar area-origins (make-array #o400))
(defvar area-alloc-pointers (make-array #o400))
(defvar area-alloc-bounds (make-array #o400))

(defvar area-corresponding-arrays
	'(sym:area-name sym:region-origin sym:region-length
			sym:region-free-pointer
	      sym:region-gc-pointer sym:region-bits sym:area-region-list ; area-region-bits
	      sym:area-region-size sym:area-maximum-size sym:region-list-thread))

(defvar micro-code-entry-corresponding-arrays
	'(sym:micro-code-entry-area
	      sym:micro-code-entry-name-area
	      sym:micro-code-entry-args-info-area
	      sym:micro-code-entry-arglist-area
	      sym:micro-code-entry-max-pdl-usage))

(defvar areas-with-fill-pointers
	(append '(sym:micro-code-symbol-area
		   sym:micro-code-symbol-name-area
		   sym:support-entry-vector
		   sym:constants-area)
		area-corresponding-arrays micro-code-entry-corresponding-arrays))

;;; areas in this list get art-q-list
(defvar list-referenced-areas areas-with-fill-pointers)

;;; areas in this list get art-q, all other areas get art-32b
(defvar array-referenced-areas '(sym:system-communication-area sym:page-table-area))

(defun create-areas (&aux high-loc the-region-bits)
  (do ((l sym:cold-load-area-sizes (cddr l)))	;Area sizes in pages
      ((null l) t)
    (setf (get (car l) 'area-size) (cadr l)))
  ;(fillarray area-origins '(nil))
  (dotimes (x (array-dimension area-origins 0))
	   (setf (aref area-origins x) nil))
  ;; Set up the area origin and allocation tables
  (loop with quantum = sym:page-size
	for area in sym:area-list
	for area-number from 0 by 1
	for loc = 0 then (+ loc size)
	as size = (* (ceiling (* (get-area-size area) sym:page-size) quantum)
		     quantum)
	when (eq area 'sym:init-list-area)	;Last fixed area
	  do (setq quantum sym:%address-space-quantum-size)
	     (let ((foo (rem (+ loc size) quantum)))	;Start next area on quantum boundary
	       (or (zerop foo) (setq size (+ (- size foo) quantum))))
	do (setf (aref area-origins area-number) loc)
	finally (setq high-loc loc))
  ;(copy-array-contents area-origins area-alloc-pointers)
  (dotimes (x (array-dimension area-origins 0))
	   (setf (aref area-alloc-pointers x) (aref area-origins x)))
  ;(copy-array-portion area-origins 1 #o400 area-alloc-bounds 0 #o400)
  (dotimes (x (1- (array-dimension area-origins 0)))
	   (setf (aref area-alloc-bounds x) (aref area-origins (1+ x))))
  (setf (aref area-alloc-bounds (1- (length sym:area-list))) high-loc)
  (setq vmem-highest-address high-loc)
  (vmem-initialize)
  ;; Fill various areas with default stuff
  (init-area-contents 'sym:area-region-size (vfix #o40000))
  (init-area-contents 'sym:area-maximum-size (vfix big-fixnum))
  (init-area-contents 'sym:region-origin (vfix 0))  ;so good type in free region#'s
  (init-area-contents 'sym:region-length (vfix 0))  ;..
  (init-area-contents 'sym:region-free-pointer (vfix 0))
  (init-area-contents 'sym:region-gc-pointer (vfix 0))
  (init-area-contents 'sym:region-bits (vfix 0))  ;Suitable for free region
  ; (init-area-contents 'sym:area-region-bits (vfix 0))
  ;; Crank up region size for certain big areas
  (do ((l sym:cold-load-region-sizes (cddr l)))
      ((null l) nil)
      (vwrite (+ (get-area-origin 'sym:area-region-size) (get-area-number (car l)))
	      (vfix (cadr l))))
  ;; Set up contents of certain initial areas
  (do ((i 0 (1+ i))
       (al sym:area-list (cdr al))
       (fixed-p t))
      ((null al) nil)
    (and (eq (car al) 'sym:working-storage-area) (setq fixed-p nil))
    (vwrite (+ (get-area-origin 'sym:area-region-list) i) (vfix i))
    (vwrite (+ (get-area-origin 'sym:region-list-thread) i) (vfix (+ i little-fixnum)))
    (vwrite (+ (get-area-origin 'sym:region-bits) i)
	    (setq the-region-bits 
		  (vfix (+ (lispm-dpb (cond ((memq (car al) sym:read-only-area-list) #o1200)	;ro
				((memq (car al) sym:wired-area-list) #o1400)	;rw
				((memq (car al) sym:pdl-buffer-area-list)
				 #o500)			;may be in pdl-buffer, no access.
				(t #o1300))			;rwf
			  sym:%%region-map-bits
			  0)
		     (lispm-dpb 1 sym:%%region-oldspace-meta-bit 0)
		     (lispm-dpb (if (eq (car al) 'sym:extra-pdl-area) 0 1)
			  sym:%%region-extra-pdl-meta-bit 0)
		     (lispm-dpb (if (memq (car al) sym:list-structured-areas) 0 1)
			  sym:%%region-representation-type 0)
		     (lispm-dpb (cond ((eq (car al) 'sym:extra-pdl-area)
				 sym:%region-space-extra-pdl)
				(fixed-p sym:%region-space-fixed)
				((memq (car al) sym:static-areas) sym:%region-space-static)
				(t sym:%region-space-new))
			  sym:%%region-space-type 0)
		     ;; Set up the scavenge enable.  Note!  The extra-pdl does not follow the
		     ;; prescribed protocol for header/body forward, and gets randomly reset.
		     ;; Fortunately it never points at anything.
		     (lispm-dpb (cond ((eq (car al) 'sym:extra-pdl-area) 0)
				(fixed-p	;These usually should be scavenged, except
						;for efficiency certain ones
						;that only contain fixnums will be bypassed
				 (if (memq (car al)
					   '(sym:micro-code-symbol-area sym:page-table-area
						 sym:physical-page-data sym:region-origin
						 sym:region-length sym:region-bits sym:region-free-pointer
						 sym:address-space-map
						 sym:region-gc-pointer sym:region-list-thread
						 sym:area-region-list
						 ; sym:area-region-bits
						 sym:area-region-size sym:area-maximum-size
						 sym:micro-code-entry-area
						 sym:micro-code-entry-max-pdl-usage))
				     0 1))
				((memq (car al) sym:static-areas) 1)	;Static needs scav
				(t 0))		;Newspace doesn't need scavenging
			  sym:%%region-scavenge-enable 0)
		     ))))
;    (vwrite (+ (get-area-origin 'sym:area-region-bits) i)
;	    the-region-bits)
    (vwrite (+ (get-area-origin 'sym:region-origin) i)
	    (vfix (aref area-origins i)))
    (vwrite (+ (get-area-origin 'sym:region-length) i)
	    (vfix (- (aref area-alloc-bounds i) (aref area-origins i))))))

(defun get-area-number (area)
  (cond ((numberp area) area)
	((position area sym:area-list))	;No symeval, the might have changed.
	((error "~S bad area-name" area))))

(defun get-area-origin (area)
  (aref area-origins (get-area-number area)))

(defun get-area-bound (area)
  (aref area-alloc-bounds (get-area-number area)))

(defun get-area-free-pointer (area)
  (aref area-alloc-pointers (get-area-number area)))

(defun allocate-block (area size &aux address high)
  (setq area (get-area-number area))
  (setq address (aref area-alloc-pointers area))
  (setq high (+ address size))
  (and (> high (aref area-alloc-bounds area))
       (error "~A area overflow" (nth area sym:area-list)))
  (setf (aref area-alloc-pointers area) high)
  ;Page in all the fresh pages without really paging them in, thus initializing them
  (do ((vpn (ceiling address sym:page-size) (1+ vpn))
       (hpn (ceiling high sym:page-size)))
      ((>= vpn hpn))
    (vmem-find-page (* vpn sym:page-size) t))
  address)

;;; In pages
(defun get-area-size (area)
  ;(check-arg area (memq area sym:area-list) "an area-name")
  (cond ((get area 'area-size))
	(t 1)))

;;; Doesn't advance allocation pointer, i.e. sets it back to origin when done
(defun init-area-contents (area contents)
  (let ((count (* sym:page-size (get-area-size area))))
    (setq area (get-area-number area))
    (do ((adr (allocate-block area count) (1+ adr))
	 (n count (1- n)))
	((zerop n)
	 (store-nxtnil-cdr-code area)
	 (setf (aref area-alloc-pointers area) (aref area-origins area)))
      (vwrite-cdr adr sym:cdr-next contents))))

(defvar store-halfwords-address)
(defvar store-halfwords-count)
(defvar store-halfwords-buffer)

(defun begin-store-halfwords (name-of-area n-words)
  (let* ((area-number (get-area-number name-of-area))
	 (address (allocate-block area-number n-words)))
    (setq store-halfwords-address address
	  store-halfwords-count (* 2 n-words))
    address))

(defun store-halfword (hwd)
  (cond ((oddp (setq store-halfwords-count (1- store-halfwords-count)))
	 (setq store-halfwords-buffer hwd))
	(t 
	 (vwrite store-halfwords-address (lispm-dpb hwd #o2020 store-halfwords-buffer))
	 (setq store-halfwords-address (1+ store-halfwords-address)))))

(defun end-store-halfwords ()
  (or (zerop store-halfwords-count)
      (error "store-halfword called wrong number of times")))

;;; Given an object in our world, construct a matching one in the cold load world
;;; and return a cold-load pointer to it.
(defun make-q-list (area s-exp &aux bsize value)
  (cond ((numberp s-exp)
	 (cond ;((small-floatp s-exp) (make-small-flonum s-exp))
	       ((floatp s-exp) (store-flonum 'sym:working-storage-area s-exp))
	       ((and (<= s-exp big-fixnum) (>= s-exp little-fixnum)) (vfix s-exp))
	       (t (store-bignum 'sym:working-storage-area s-exp))))
;	((characterp s-exp)
;	 (vmake-pointer sym:dtp-character (char-int s-exp)))
	((symbolp s-exp) (qintern s-exp))
	((stringp s-exp) (store-string 'sym:p-n-string s-exp))
	((atom s-exp) (error "~S unknown type" s-exp))
	(t (or (memq area sym:list-structured-areas)
	       (error "make-q-list in non-list-structured area ~S" area))
	   (setq bsize (lispm-length s-exp))
	   ; This will break if it isn't a pure dotted list
	   (cond ((cdr (last s-exp))
		  (setq bsize (1+ bsize)))	;ends in dotted pair
		 (t (setq bsize (length s-exp))))
	   (setq value (vmake-pointer sym:dtp-list (allocate-block area bsize)))
	   (do ((s-exp s-exp (cdr s-exp))
		(adr (logand q-pointer-mask value) (1+ adr))
		(c-code))
	       ((atom s-exp)
		(or (null s-exp)
		    (vwrite-cdr adr sym:cdr-error (make-q-list area s-exp))))
	     (setq c-code (cond ((null (cdr s-exp)) sym:cdr-nil)
				((atom (cdr s-exp)) sym:cdr-normal)
				(t sym:cdr-next)))
	     (vwrite-cdr adr c-code (make-q-list area (car s-exp))))
	   value)))

(defun make-small-flonum (s-exp)  ;I hope the format doesn't change!
  (let ((as-fixnum (%pointer s-exp)))
;; The following line should be removed once we are running in system 99 or above.
   ;(setq as-fixnum (%pointer-plus as-fixnum #o40000000))
    (vmake-pointer sym:dtp-small-flonum as-fixnum)))

(defun magic-aref (a i n)
  (if (< i n) (char-code (aref a i)) #o200))

(defun store-string (area string)
   (and (memq area sym:list-structured-areas)
	(error "store-string in list-structured area"))
   (let* ((n-chars (length string))
	  (n-words (+ 1 (ceiling n-chars 4)))
	  long-flag
	  adr)
     (and (> n-chars sym:%array-max-short-index-length)
	  (setq long-flag t
		n-words (1+ n-words)))
     (setq adr (allocate-block area n-words))
     (vwrite adr (vmake-pointer sym:dtp-array-header
				(+ sym:array-dim-mult	;1-dim
				   sym:art-string
				   (if long-flag
				       (lispm-dpb 1 sym:%%array-long-length-flag 0)
				     n-chars))))
     (when long-flag
       (vwrite (1+ adr) n-chars))
     (do ((i (if long-flag 2 1) (1+ i))
	  (j 0 (+ j 4)))
	 ((= i n-words))
       (vwrite (+ adr i)
	       (+ (magic-aref string j n-chars)
		  (ash (magic-aref string (1+ j) n-chars) 8)
		  (ash (magic-aref string (+ j 2) n-chars) 16)
		  (ash (magic-aref string (+ j 3) n-chars) 24))))
     (vmake-pointer sym:dtp-array-pointer adr)))

(defun store-symbol-vector (atom-name area)
  (and (memq area sym:list-structured-areas)
       (error "store-symbol-vector in list-structured area ~S" area))
  (and (eq atom-name '**screw**)
       (error "you've probably encountered a bug in COLDLD" atom-name))
  (prog (adr sym path real-atom-name package-name pname)
     (cond ((setq path (get atom-name 'package-path))
	    (or (= (length path) 2)
		(error "package path ~S not 2 long - code not hairy enough"))
	    (setq package-name (qintern (car path))
		  real-atom-name (car (last path))))
	   (t (setq package-name qnil real-atom-name atom-name)))
     (when symbol-creation-trace-list	;debugging tool to track down appears twice in 
       (do ((l symbol-creation-trace-list (cdr l)))	;cold load messages.
	   ((null l))
	 (cond ((inhibit-style-warnings
		  (samepnamep real-atom-name (car l)))
		(format t "
A-flavor-of ~S being-created, atom-name ~S, path ~S, package-name ~S"
			real-atom-name atom-name path  package-name)))))
     (setq pname (store-string 'sym:p-n-string (string real-atom-name)))
     (setq adr (allocate-block area sym:length-of-atom-head))
     (vwrite-cdr adr sym:cdr-next (vmake-pointer sym:dtp-symbol-header pname))
     (vwrite-cdr (+ adr 1) sym:cdr-next (vmake-pointer sym:dtp-null adr))
     (vwrite-cdr (+ adr 2) sym:cdr-next (vmake-pointer sym:dtp-null adr))
     (vwrite-cdr (+ adr 3) sym:cdr-next qnil)
     (vwrite-cdr (+ adr 4) sym:cdr-nil package-name)
     (setq sym (vmake-pointer sym:dtp-symbol adr))
     (setf (get atom-name 'q-atom-head) sym)
     (return sym)))

;;; Need to explicitly create a Lispm format bignum when running in
;;; Common Lisp

(defun store-bignum (area number)
  (and (memq area sym:list-structured-areas)
       (error "extended-number in list-structured area ~S" area))
  (let* ((size 3) ; XXX was (%structure-total-size number)
	 (adr (allocate-block area size)))
    (vwrite-cdr adr sym:cdr-nil
		(lispm-dpb sym:dtp-header sym:%%q-all-but-pointer
			   (lispm-dpb sym:%header-type-bignum
				      sym:%%header-type-field (1- size))))
    (vwrite (+ adr 1) (ldb (byte 31 0) number))
    (vwrite (+ adr 2) (ldb (byte 31 0) (ash number -31)))
    (vmake-pointer sym:dtp-extended-number adr)))

(defun store-flonum (area number)
  (and (memq area sym:list-structured-areas)
       (error "extended-number in list-structured area ~S" area))
  (format t "store-flonum: ~S ~F~%" (type-of number) number)
  (let* ((size 2) ; XXX (%structure-total-size number)
	 (adr (allocate-block area size)))
    (vwrite-cdr adr sym:cdr-nil
		(lispm-dpb sym:dtp-header sym:%%q-all-but-pointer
			   (lispm-dpb sym:%header-type-flonum
				      sym:%%header-type-field (1- size))))
    ;(vwrite (+ adr 1) (ldb (byte 31 0) number))
    (vmake-pointer sym:dtp-extended-number adr)))

;;; New version of qintern.  Machine builds obarray when it first comes up (easy enough).
(defun qintern (atom-name)
    (or (eq (symbol-package atom-name) sym-package)
	(setq atom-name (intern (string atom-name) sym-package)))
    (or (get atom-name 'q-atom-head)
	(store-symbol-vector atom-name 'sym:nr-sym)))

(defun q-atom-head-reset (&optional (pkg sym-package))
  (do-symbols (x pkg) (remprop x 'q-atom-head)))

(defun print-q-symbols (&optional (pkg sym-package))
  (do-symbols (x pkg)
	      (let ((q-atom (get x 'q-atom-head)))
		(if q-atom
		    (format t "~%Symbol ~s, q-atom-head ~s" x q-atom)))))

(defun store-nxtnil-cdr-code (area)
  (vstore-cdr-code (1- (aref area-alloc-pointers (get-area-number area))) sym:cdr-nil))

(defun store-list-of-atoms (area loa)
  (let ((adr (allocate-block area (length loa))))
    (do ((loa loa (cdr loa))
	 (adr adr (1+ adr)))
	((null loa))
      (vwrite-cdr adr (if (null (cdr loa)) sym:cdr-nil sym:cdr-next)
		      (q-convert-atom (car loa))))
    adr))

(defun q-convert-atom (atm)
  (if (numberp atm) (make-q-list nil atm) (qintern atm)))

(defun store-list (area lst)
  (let ((adr (allocate-block area (length lst))))
    (do ((lst lst (cdr lst))
	 (adr adr (1+ adr)))
	((null lst))
      (vwrite-cdr adr (if (null (cdr lst)) sym:cdr-nil sym:cdr-next)
		      (make-q-list 'sym:init-list-area (car lst))))
    adr))

(defun store-nils (area number)
  (let ((adr (allocate-block area number)))
    (do ((number number (1- number))
	 (adr adr (1+ adr)))
	((zerop number))
      (vwrite-cdr adr (if (= number 1) sym:cdr-nil sym:cdr-next) qnil))
    adr))

(defun storeq (area data)
  (let ((adr (allocate-block area 1)))
    (vwrite adr data)
    adr))

(defun store-cdr-q (area cdr-code data)
  (let ((adr (allocate-block area 1)))
    (vwrite-cdr adr cdr-code data)
    adr))

;;;; Hair for making arrays

(defun init-q-array (area name offset type dimlist displaced-p leader)
  (init-q-array-named-str area name offset type dimlist displaced-p leader nil))

;NOTE!! LEADER IS STOREQ ED DIRECTLY SO IT MUST ALREADY BE MAKE-Q-LIST IFIED
(defun init-q-array-named-str (area name offset type dimlist displaced-p leader named-str)
	;  leader is contents of array leader, if desired.  it is in "storage order"
	;which is reversed from index order.
	;  if leader is numeric, it means make leader consisting of that many q's
	;initialized to nil.
	;  if name -> nil, return (list <array-adr> <data-length>) and dont try
	;to store in function or value cell.
	;offset 1 for storing pointer to array in value cell, 2 for function cell
  (and (memq area sym:list-structured-areas)
       (error "init-q-array in list-structured area"))
  (prog (tem ndims index-length data-length tem1 leader-length header-q long-array-flag adr)
	(and (numberp dimlist) (setq dimlist (list dimlist)))
	(setq ndims (length dimlist))
	(when sym:new-array-index-order
	  (setq dimlist (reverse dimlist)))
	;; The rest of this is correct for column-major order.
	(setq index-length (list-product dimlist))
	(cond ((and (> index-length sym:%array-max-short-index-length)
		    (null displaced-p))
	       (setq long-array-flag t)))
	(setq leader-length (cond ((null leader) 0)
				  ((numberp leader) (+ 2 leader))
				  (t (+ 2 (length leader)))))
	(cond ((null (setq tem (assoc type sym:array-elements-per-q :test #'eq)))
	       (error "~S bad array type" type)))
	(setq tem (cdr tem))
	(cond ((not (null leader))
	       (setq adr (allocate-block area leader-length))
	       (vwrite adr (vmake-pointer sym:dtp-header
					  (lispm-dpb sym:%header-type-array-leader
					       sym:%%header-type-field
					       leader-length)))
	       (cond ((numberp leader)
		      (dotimes (i leader)
			(vwrite (+ adr i 1) qnil))
		      (and named-str (vwrite (+ adr leader -1)	;(array-leader x 1)
					     (qintern named-str))))
		     (t (do ((l leader (cdr l))
			     (i 1 (1+ i)))
			    ((null l))
			  (vwrite (+ adr i) (car l)))))
	       (vwrite (+ adr leader-length -1) (vfix (- leader-length 2)))))
	(setq data-length (ceiling index-length tem))
	(setq header-q (vmake-pointer sym:dtp-array-header
				      (+ (* sym:array-dim-mult ndims)
					 (symbol-value type))))
	(and leader (setq header-q (+ header-q sym:array-leader-bit)))
	(and named-str (setq header-q (+ header-q sym:array-named-structure-flag)))
	(cond (displaced-p   ;note, no index-offset arrays in cold-load
		(setq tem 1 header-q (+ header-q sym:array-displaced-bit 2)))
	      ((null long-array-flag)
		(setq tem 1 header-q (+ header-q index-length)))
	      (t (setq tem 2 header-q (+ header-q sym:array-long-length-flag))))
	(setq tem1 (setq adr (allocate-block area (+ tem ndims -1))))
	(vwrite adr header-q)
	(and (= tem 2) (vwrite (setq adr (1+ adr)) (vfix index-length)))
	;Store all dimensions except for last
	(do ((l dimlist (cdr l)))
	    ((null (cdr l)) nil)
	  (vwrite (setq adr (1+ adr)) (vfix (car dimlist))))
	(cond ((null name) (return (list tem1 data-length))))
	(vstore-contents (+ (qintern name) offset)
			 (vmake-pointer sym:dtp-array-pointer tem1))
	(return data-length)))

(defun store-q-array-leader (arrayp idx data)
  (vwrite (- arrayp (+ 2 idx))			;1 for array header, 1 for ldr len
	  data))

;;;; Setting up various magic data structures,
;;;;  mostly having to do with the microcode and the fixed-areas

(defun store-support-vector (item)
  (let ((adr (allocate-block 'sym:support-entry-vector 1)))
    (vwrite-cdr adr sym:cdr-next
		(cond ((eq (car item) 'sym:function)
		       (get-q-fctn-cell (cadr item)))
		      ((memq (car item) '(quote sym:quote))
		       (make-q-list 'sym:init-list-area (cadr item)))
		      (t (error "bad-support-code: ~S" item))))
    adr))

(defun get-q-fctn-cell (fctn &aux tem)
  (and (setq tem (get fctn 'q-atom-head))
       (vcontents (+ tem 2))))

(defun store-displaced-array-pointer (area)
 (prog (fillp area-array-type data-length adr)
    (setq fillp (memq area areas-with-fill-pointers))
    (setq area-array-type 
	  (cond ((eq area 'sym:address-space-map) 'sym:art-8b)	;%address-space-map-byte-size
		((memq area list-referenced-areas) 'sym:art-q-list)
		((memq area array-referenced-areas) 'sym:art-q)
		(t 'sym:art-32b)))
    (init-q-array 'sym:control-tables
		  area  
		  2 
		  area-array-type  
		  (setq data-length	;In entries, not Qs!
			(if (eq area 'sym:address-space-map)
			    (/ (1+ q-pointer-mask) sym:%address-space-quantum-size)
			    (* sym:page-size (get-area-size area))))
		  t 
		  (and fillp
		       (list (vfix (cond ((memq area area-corresponding-arrays)
					  (length sym:area-list))
					 ((memq area
						micro-code-entry-corresponding-arrays)
					  (length micro-code-entry-vector))
					 ((eq area 'sym:address-space-map)
					  (/ (1+ q-pointer-mask)
						    sym:%address-space-quantum-size))
					 (t
					  (* sym:page-size (get-area-size area))))))))
    (setq adr (allocate-block 'sym:control-tables 2))
    (vwrite adr (vfix (get-area-origin area)))
    (vwrite (1+ adr) (vfix data-length))))

;;; x is a symbol or cons function-name instruction-name
(defun store-misc-link (x)
  (cond ((atom x)
	 (misc-store-micro-entry x x))
	((misc-store-micro-entry (car x) (cdr x)))))

;;; special kludge which filters out *catch 
(defun store-misc-link-1 (x)
  (or (eq x 'sym:*catch)
      (store-misc-link x)))

;;; This creates an indirect through the MICRO-CODE-SYMBOL-AREA by using
;;; DTP-FIX and #o200 less than the misc function index.  This makes
;;; the core image independent of the microcode version.
(defun misc-store-micro-entry (name me-name)
  (prog (misc-index u-entry-prop u-entry-index)
	(cond ((null (setq misc-index (get me-name 'sym:qlval)))
	       (error "No QLVAL property: ~S" me-name)))
	(setq u-entry-prop (vfix (- misc-index #o200)))
	(setq u-entry-index (get-u-entry-index name))
	(vstore-contents (+ (qintern name) 2)	;function cell
			 (vmake-pointer sym:dtp-u-entry u-entry-index))
	(vstore-contents (+ (get-area-origin 'sym:micro-code-entry-area) u-entry-index)
			 u-entry-prop)
	(vstore-contents (+ (get-area-origin 'sym:micro-code-entry-args-info-area)
			    u-entry-index)
			 (make-q-list 'sym:init-list-area (get-q-args-prop name)))))

;;; This abbreviated version of the stuff in UTIL2 should be enough to get us off the ground
(defun get-q-args-prop (fctn &aux tem)
  (cond ((setq tem (get fctn 'sym:argdesc))
	 (get-q-args-prop-from-argdesc-prop tem))
	((setq tem (get fctn 'sym:qintcmp))
	 (+ (ash tem 6) tem))
	;; You may think this is a kludge, but in the Maclisp cold-load generator
	;; it gets the number of arguments out of the Maclisp subr of the same name!
	((setq tem (get fctn 'defmic-nargs-info))
	 (if (listp tem) (+ (ash (car tem) 6) (cdr tem))
	     (+ (ash tem 6) tem)))
	(t (error "Cannot find arg desc for ~S" fctn))))

(defun get-q-args-prop-from-argdesc-prop (arg-desc)
  (prog (prop min-args max-args count item)
	(setq prop 0 min-args 0 max-args 0)
     l	(cond ((null arg-desc) (return (+ prop (ash min-args 6) max-args))))
	(setq count (caar arg-desc))
	(setq item (cadar arg-desc)) ;list of arg syntax, quote type, other attributes
	(setq arg-desc (cdr arg-desc))
     l1	(cond ((= 0 count) (go l))
	      ((memq 'sym:fef-arg-rest item)
	       (setq prop (logior prop (if (or (memq 'sym:fef-qt-eval item)
					       (memq 'sym:fef-qt-dontcare item))
					   sym:%arg-desc-evaled-rest
					   sym:%arg-desc-quoted-rest)))
	       (go l))
	      ((memq 'sym:fef-arg-req item)
	       (setq min-args (1+ min-args)))
	      ((memq 'sym:fef-arg-opt item))
	      (t (go l)))
    	(setq max-args (1+ max-args))
	(or (memq 'sym:fef-qt-eval item)
	    (memq 'sym:fef-qt-dontcare item)
	    (setq prop (logior prop sym:%arg-desc-fef-quote-hair)))
	(setq count (1- count))
	(go l1)))

(defvar micro-code-entry-vector nil)

(defun get-u-entry-index (fctn)
  (prog (tem)
	(cond ((setq tem (position fctn micro-code-entry-vector))
	       (return tem)))
	(setq tem (length micro-code-entry-vector))
	(store-cdr-q 'sym:micro-code-entry-area sym:cdr-next qnil)  ;will be changed
	(store-cdr-q 'sym:micro-code-entry-name-area sym:cdr-next (qintern fctn))
	(store-cdr-q 'sym:micro-code-entry-args-info-area sym:cdr-next qnil)  ;will be chngd
	(store-cdr-q 'sym:micro-code-entry-arglist-area sym:cdr-next qnil) ;set on startup
	(setq micro-code-entry-vector (nconc micro-code-entry-vector 
					     (list fctn)))
	(return tem)))

(defun store-micro-code-symbol-name (name)
  (let ((opcode (get name 'sym:qlval)))
    (or opcode (error "no qlval property in store-micro-code-symbol-name: ~S" name))
    (vstore-contents (+ (get-area-origin 'sym:micro-code-symbol-name-area) (- opcode 200))
		     (qintern name))))

(defun store-lisp-value-list (x)
  (mapc #'store-lisp-value (symbol-value x)))

(defun store-lisp-value (sym)
  (storein-q-value-cell sym (make-q-list 'sym:init-list-area (symbol-value sym))))

;;; Store cdr-coded list of #o1000 (or however many) NIL's.
(defun init-micro-code-symbol-name-area ()
  (store-nils 'sym:micro-code-symbol-name-area
	      (* sym:page-size
		 (get 'sym:micro-code-symbol-name-area 'area-size))))

(defun cold-load-time-setq (pair-list &aux var value)
  (do ((pair-list pair-list (cddr pair-list))) ((null pair-list))
    (setq var (car pair-list) value (cadr pair-list))
    (cond ((and (atom value) (or (numberp value)
				 (stringp value)
				 (memq value '(sym:t sym:nil)))))
	  ((eq (car value) 'sym:quote)
	   (setq value (cadr value)))
	  (t (error "(setq ~S ~S) no can do" var value)))
    (storein-q-value-cell var (make-q-list 'sym:init-list-area value))))

(defun storein-q-value-cell (sym data)
  (vstore-contents (1+ (qintern sym)) data))

(defun store-constant (c)
  (vwrite-cdr (allocate-block 'sym:constants-area 1)
	      sym:cdr-next
	      (make-q-list 'sym:init-list-area c)))

(defun init-scratch-pad-area ()
  (init-area-contents 'sym:scratch-pad-init-area (vfix 0))
  (setf (aref area-alloc-pointers (get-area-number 'sym:scratch-pad-init-area))
	(+ (aref area-origins (get-area-number 'sym:scratch-pad-init-area))
	   sym:scratch-pad-parameter-offset 
	   (length sym:scratch-pad-parameters)))
  (scratch-store-q 'sym:initial-top-level-function
		   (vmake-pointer sym:dtp-locative
				  (+ (qintern 'sym:lisp-top-level) 2)))
  ;trap-handler (not used)
  (let ((initial-stack-group-pointer (make-initial-stack-group-structure)))
    (scratch-store-q 'sym:current-stack-group initial-stack-group-pointer)
    (scratch-store-q 'sym:initial-stack-group initial-stack-group-pointer))
  (scratch-store-q 'sym:error-handler-stack-group qnil)  ;initialized at run time
  (scratch-store-q 'sym:default-cons-area (vfix (get-area-number 'sym:working-storage-area))))

(defun scratch-store-q (symbolic-name data)
   (prog (tem origin)
	 (setq origin (get-area-origin 'sym:scratch-pad-init-area))
	 (cond ((setq tem (position symbolic-name sym:scratch-pad-pointers))
		(vstore-contents (+ origin tem) data))
	       ((setq tem (position symbolic-name sym:scratch-pad-parameters))
		(vstore-contents (+ origin sym:scratch-pad-parameter-offset tem) data))
	       (t (error "unknown-scratch-quantity: ~S" symbolic-name)))))

(defun store-a-mem-location-names ()
    (do ((name sym:a-memory-location-names (cdr name))
	 (locn (+ #o40 sym:a-memory-virtual-address) (1+ locn)))
	((null name) t)
	(store-mem-location (car name) locn))
    (do ((name sym:m-memory-location-names (cdr name)))
	((null name) t)
	(store-mem-location (car name) (get (car name) 'sym:forwarding-virtual-address)))
    (store-mem-location 'sym:%gc-generation-number
			(+ #o400 sym:%sys-com-gc-generation-number))
    )

(defun store-mem-location (name locn)
  (storein-q-value-cell name (vmake-pointer sym:dtp-one-q-forward locn)))

(defun make-ordered-array-list (assoc-list)
  (mapcar #'(lambda (x) (cdr (assoc x assoc-list :test #'eq)))
	  sym:array-types))

;;;The order store-misc-link is called determines the final micro-code-entry
;;; numbers that are assigned.  however, except for 0 which must be *catch,
;;; micro-code-entry numbers are unconstrained and independant from everything
;;; else.  So the other entries below may be in any order.
(defun store-misc-u-entry-links ()
  (store-misc-link 'sym:*catch)		;must be first
  (mapc #'store-misc-link-1 misc-function-list)
  ;; now set up the first #o600 locations of micro-code-symbol-name-area
  (init-micro-code-symbol-name-area)
  (mapc #'store-micro-code-symbol-name misc-instruction-list))

(defun make-initial-stack-group-structure ()
  (make-stack-group-structure 'sym:main-stack-group 'sym:control-tables
			      'sym:linear-pdl-area 'sym:linear-bind-pdl-area
			      sym:sg-state-active))
 
(defun make-stack-group-structure (name sg-area linear-area l-b-p-area initial-state) 
  (prog (sg pdl-array l-b-p-array reg-len spec-len)
	(setq sg (car (init-q-array sg-area nil nil 'sym:art-stack-group-head '(0)
				    nil (length sym:stack-group-head-leader-qs))))
	(setq pdl-array
	      (car (init-q-array linear-area nil nil 'sym:art-reg-pdl
			     (list (setq reg-len (- (* sym:page-size
						       (get-area-size 'sym:linear-pdl-area))
						    (+ (length sym:reg-pdl-leader-qs) 4))))
			;4: leader header + leader-length-q + array-header-q + long-length-q
			     nil (length sym:reg-pdl-leader-qs))))
	(allocate-block linear-area reg-len)	;advance free pointer
	(setq l-b-p-array
	      (car (init-q-array l-b-p-area nil nil 'sym:art-special-pdl
		        (list (setq spec-len (- (* sym:page-size
						   (get-area-size 'sym:linear-bind-pdl-area))
						(+ (length sym:special-pdl-leader-qs) 4))))
			nil (length sym:special-pdl-leader-qs))))
	(allocate-block l-b-p-area spec-len)	;advance free pointer
	(stack-group-linkup sg pdl-array l-b-p-array)
	(store-q-array-leader sg sym:sg-state (vfix initial-state))
	(store-q-array-leader sg sym:sg-name (make-q-list 'sym:init-list-area name))
	(store-q-array-leader sg sym:sg-regular-pdl-limit
			      (make-q-list 'sym:init-list-area (- reg-len #o100)))
	(store-q-array-leader sg sym:sg-special-pdl-limit
			      (make-q-list 'sym:init-list-area (- spec-len #o100)))
	(return (vmake-pointer sym:dtp-stack-group sg))))

(defun stack-group-linkup (sg pdl-arrayp l-b-p-arrayp)
  (store-q-array-leader l-b-p-arrayp sym:special-pdl-sg-head-pointer
			(vmake-pointer sym:dtp-stack-group sg))
  (store-q-array-leader pdl-arrayp sym:reg-pdl-sg-head-pointer
			(vmake-pointer sym:dtp-stack-group sg))
  (store-q-array-leader sg sym:sg-special-pdl
			(vmake-pointer sym:dtp-array-pointer l-b-p-arrayp))
  (store-q-array-leader sg sym:sg-regular-pdl
			(vmake-pointer sym:dtp-array-pointer pdl-arrayp))
  (store-q-array-leader sg sym:sg-initial-function-index (vfix 3)))

;This better agree with the order of the list of qs in QCOM
(defun init-system-communication-area (&aux (nqs 27.) adr)
  (setq adr (allocate-block 'sym:system-communication-area nqs))
  (vwrite (+ adr sym:%sys-com-area-origin-pntr)
	  (vmake-pointer sym:dtp-locative (get-area-origin 'sym:region-origin)))
  (vwrite (+ adr sym:%sys-com-valid-size) (vfix 0))	;fixed later
  (vwrite (+ adr sym:%sys-com-page-table-pntr)
	  (vmake-pointer sym:dtp-locative (get-area-origin 'sym:page-table-area)))
  (vwrite (+ adr sym:%sys-com-page-table-size)	;Real value put in by microcode
	  (vfix (* (get-area-size 'sym:page-table-area) sym:page-size)))
  (vwrite (+ adr sym:%sys-com-obarray-pntr) (qintern 'sym:obarray))
  (vwrite (+ adr sym:%sys-com-ether-free-list) qnil)
  (vwrite (+ adr sym:%sys-com-ether-transmit-list) qnil)
  (vwrite (+ adr sym:%sys-com-ether-receive-list) qnil)
  (vwrite (+ adr sym:%sys-com-band-format) (vfix 0))	;not compressed format
  (vwrite (+ adr sym:%sys-com-gc-generation-number) (vfix 0))
  (vwrite (+ adr sym:%sys-com-unibus-interrupt-list) (vfix 0))
  (vwrite (+ adr sym:%sys-com-temporary) (vfix 0))
  (vwrite (+ adr sym:%sys-com-free-area#-list) 0)	;fixed later
  (vwrite (+ adr sym:%sys-com-free-region#-list) 0)	;fixed later
  (vwrite (+ adr sym:%sys-com-memory-size) (vfix #o100000))	;assume 32k, fixed later
  (vwrite (+ adr sym:%sys-com-wired-size)  ;region-gc-pointer is the first pageable area
	  (vfix (get-area-origin 'sym:region-free-pointer)))
  (vwrite (+ adr sym:%sys-com-chaos-free-list) qnil)
  (vwrite (+ adr sym:%sys-com-chaos-transmit-list) qnil)
  (vwrite (+ adr sym:%sys-com-chaos-receive-list) qnil)
  (vwrite (+ adr sym:%sys-com-debugger-requests) (vfix 0))
  (vwrite (+ adr sym:%sys-com-debugger-keep-alive) (vfix 0))
  (vwrite (+ adr sym:%sys-com-debugger-data-1) (vfix 0))
  (vwrite (+ adr sym:%sys-com-debugger-data-2) (vfix 0))
  (vwrite (+ adr sym:%sys-com-major-version) qnil)	;I.e. fresh cold-load
  (vwrite (+ adr sym:%sys-com-desired-microcode-version) qnil)	;Set by system initialization
  (vwrite (+ adr sym:%sys-com-highest-virtual-address)
	  (vfix 0)) ;used only if compressed band.
  (vwrite (+ adr sym:%sys-com-pointer-width) (vfix sym:%%q-pointer))
  (or (= nqs (length sym:system-communication-area-qs))
      (error "QCOM and COLDUT disagree about system-communication-area")))

(defun q-storage-finalize ()
  (mapc #'store-support-vector sym:support-vector-contents)
  (store-nxtnil-cdr-code 'sym:support-entry-vector)
  (mapc #'store-displaced-array-pointer sym:area-list)
  (scratch-store-q 'sym:active-micro-code-entries (vfix (length micro-code-entry-vector)))
  ;; Transfer over free pointers
  (do ((area-number 0 (1+ area-number))
       (a-l sym:area-list (cdr a-l))
       (rfp (get-area-origin 'sym:region-free-pointer)))
      ((null a-l) t)
    (vwrite (+ rfp area-number)
	    (vfix (- (aref area-alloc-pointers area-number) (aref area-origins area-number))))
    )

  (let ((high-loc (aref area-alloc-bounds (1- (length sym:area-list)))))
    (vwrite (+ (get-area-origin 'sym:system-communication-area) sym:%sys-com-valid-size)
	    (vfix high-loc)))
  ;; Set up the area# and region# free lists
  (vwrite (+ (get-area-origin 'sym:system-communication-area) sym:%sys-com-free-area#-list)
	  (vfix (length sym:area-list)))
  (vwrite (+ (get-area-origin 'sym:system-communication-area) sym:%sys-com-free-region#-list)
	  (vfix (length sym:area-list)))
  (do ((i (length sym:area-list) (1+ i)))
      ((= i sym:size-of-area-arrays) nil)	;all but the last
    (vwrite (+ (get-area-origin 'sym:region-list-thread) i) (vfix (1+ i)))
    (vwrite (+ (get-area-origin 'sym:area-region-list) i) (vfix (1+ i))))
  (vwrite (+ (get-area-origin 'sym:region-list-thread) sym:size-of-area-arrays) (vfix 0))
  (vwrite (+ (get-area-origin 'sym:area-region-list) sym:size-of-area-arrays) (vfix 0))
  ;; Make certain areas look full
  (dolist (area '(sym:region-origin sym:region-length sym:region-free-pointer sym:region-gc-pointer
		      sym:region-bits sym:region-list-thread sym:area-name sym:area-region-list
		      ; sym:area-region-bits
		      sym:area-region-size sym:area-maximum-size 
		      sym:linear-pdl-area sym:linear-bind-pdl-area))
    (vwrite (+ (get-area-origin 'sym:region-free-pointer) (get-area-number area))
	    (vfix (* (get-area-size area) sym:page-size))))
  ;; Initialize unused portions of the disk
  (initialize-unused-pages)
  (init-address-space-map)
  ;; Don't bother setting up the PHT and PPD, the microcode will take care of it
  ;; Cold-booting into this band will then do the right thing with it
  (init-area-contents 'sym:page-table-area (vfix 0))
  ;; Terminate areas which have overlying lists
  (store-nxtnil-cdr-code 'sym:constants-area)
  (store-nxtnil-cdr-code 'sym:scratch-pad-init-area)
  (store-nxtnil-cdr-code 'sym:area-name)
  (store-nxtnil-cdr-code 'sym:micro-code-entry-area)
  (store-nxtnil-cdr-code 'sym:micro-code-entry-name-area)
  (store-nxtnil-cdr-code 'sym:micro-code-entry-args-info-area)
  (store-nxtnil-cdr-code 'sym:micro-code-entry-arglist-area))

(defun initialize-unused-pages (&aux area address high)
  (dolist (name-of-area (memq 'sym:extra-pdl-area sym:area-list)) ;no trash low fixed areas
    (setq area (get-area-number name-of-area)
	  address (aref area-alloc-pointers area)
	  high (aref area-alloc-bounds area))
    ;Page in all the fresh pages without really paging them in, thus initializing them
    (do ((vpn (ceiling address sym:page-size) (1+ vpn))
	 (hpn (ceiling high sym:page-size)))
	((>= vpn hpn))
	(vmem-find-page (* vpn sym:page-size) t))))

(defun init-address-space-map ()
  (or (= sym:%address-space-map-byte-size 8)
      (error "This code only works for %address-space-map-byte-size = 8"))
 (let ((map (make-array #o2000 :element-type '(unsigned-byte 8))) ;Initializes to 0
	(asm (get-area-origin 'sym:address-space-map))
	(asqs sym:%address-space-quantum-size))
    ;For each non-fixed area, find all the address space quanta in the area's initial
    ;region and store them into the map
    (loop for area from (1+ (get-area-number 'sym:init-list-area))
		   below (length sym:area-list)
	  unless (and (zerop (rem (aref area-origins area) asqs))
		      (zerop (rem (aref area-alloc-bounds area) asqs)))
	    do (error "Area ~A is not an integral number of address space quanta"
			   (nth area sym:area-list))
	  do (loop for q from (/ (aref area-origins area) asqs)
			 below (/ (aref area-alloc-bounds area) asqs)
		   do (setf (aref map q) area)))
    ;Now dump this into the cold load
    (loop for i from 0 below #o400 for j from 0 by 4
	  do (vwrite (+ asm i)
		     (lispm-dpb (aref map (+ j 3)) #o3010
			  (lispm-dpb (aref map (+ j 2)) #o2010
			       (lispm-dpb (aref map (+ j 1)) #o1010
				    (aref map j))))))
    ;cause address-space-map region to appear full so it gets dumped by band dumper.
    (vwrite (+ (get-area-origin 'sym:region-free-pointer)
	       (get-area-number 'sym:address-space-map))
	    (vfix (* (get-area-size 'sym:address-space-map) sym:page-size)))))

(defun make-sorted-region-list ()
  (sort (do ((i 0 (1+ i))
	     (al sym:area-list (cdr al))
	     (l nil))
	    ((null al)
	     (nreverse l))
	  (push (cons (aref area-origins i) i) l))
	#'(lambda (x y)
	    (cond ((= (car x) (car y))		;if one is zero length, it -must- go first
		   (cond
		     ((= (aref area-origins (cdr x)) (aref area-alloc-bounds (cdr x))) t)
		     ((= (aref area-origins (cdr y)) (aref area-alloc-bounds (cdr y))) nil)
		     (t (error "2 non-zero-length areas at same address"))))
		  ((< (car x) (car y)))))))

;;;; Driver

(defvar cold-list-area 'sym:init-list-area)	;Where FROID (COLDLD) puts lists (usually)
(defvar evals-to-be-sent-over)

;;; User calls this to build a cold-load into a file
(defun make-cold (part-name)
  (if (numberp part-name) (setq part-name (format nil "LOD~d" part-name)))
  (or (boundp 'big-fixnum) (load-parameters))
  ;; Flush old state
  (do-symbols (x sym-package) (remprop x 'q-atom-head))
  (q-atom-head-reset)
  ;;(q-atom-head-reset (pkg-find-package "GLOBAL"))
  (makunbound 'cold-loaded-file-property-lists)
  (makunbound 'cold-loaded-function-property-lists)
  (setq evals-to-be-sent-over nil)
  (unwind-protect (make-cold-1 +cold-load-file-list+)
    (vmem-finish part-name)))

(defun make-cold-1 (file-list)
  ;; Divide up virtual memory into areas and initialize tables
  (assign-values sym:area-list 0)
  (create-areas)
  (make-t-and-nil)
  ;; Initialize various fixed areas and really random data tables
  (init-area-contents 'sym:area-name qnil)
  (store-list-of-atoms 'sym:area-name sym:area-list)
  (mapc #'store-constant sym:constants-page)	;set up constants page
  (storein-q-value-cell 'sym:constants-page
			(vmake-pointer sym:dtp-list (get-area-origin 'sym:constants-area)))
  (init-scratch-pad-area)
  (init-system-communication-area)
  (fix-certain-variables)
  (mapc #'store-lisp-value-list sym:q-corresponding-variable-lists)
  (init-random-variables)
  (store-a-mem-location-names)
  (setq micro-code-entry-vector nil)
  (store-misc-u-entry-links)
  ;A copy of AREA-LIST was previously sent over.  Change it to share with AREA-NAME.
  (storein-q-value-cell 'sym:area-list
			(vmake-pointer sym:dtp-list (get-area-origin 'sym:area-name)))
  ;;Load up all those QFASL files
  (mapc #'cold-fasload file-list)
  ;;Don't let list-structure portion of the readtable end up in a read-only area
  (let ((cold-list-area 'sym:property-list-area))  ;Random list-structured area
    (cold-fasload "sys:io;rdtbl.qfasl")
    ;(cold-fasload "sys:io;crdtbl.qfasl") ; sys99
    )
  ;;Translate all pathnames needed before logical pathnames work
  (dolist (sym mini-file-alist-list)
    (storein-q-value-cell
      sym
      (make-q-list 'sym:init-list-area
		   (loop for (file pack) in (symbol-value sym)
			 as pathname = (merge-pathnames file)
			 collect (list (namestring (translate-logical-pathname pathname))
				       pack
				       (equal (pathname-type pathname) "QFASL")
				       )))))
#|  ;MACRO, SETQ, etc. are in QFCTNS, which is now in the cold load.
  ;;THIS KLUDGE FIXES UP MACROS, SINCE THE FUNCTION MACRO IS NOT DEFINED YET
  ;;(BY SPECIAL DISPENSATION WE HAVE DEFPROP, PUTPROP, AND SPECIAL AROUND)
  ;;FURTHERMORE, SETQ ISN'T DEFINED YET, LOAD-TIME-SETQ FASL-OP SHOULD HAVE BEEN USED
  (do ((l evals-to-be-sent-over (cdr l))) ((null l))
    (cond ((memq (caar l) '(sym:setq sym:and sym:or sym:cond))
	   (error "~A will get undefined function during initialization" (car l)))
	  ((eq (caar l) 'sym:macro)
	   (rplaca l (sublis (list (cons 'fcn (cadar l))
				   (cons 'name (caddar l))
				   (cons 'body (cdddar l)))
			     '(sym:fset (sym:quote fcn)
					(sym:quote (sym:macro
						     . (sym:lambda name . body)))))))))
|#
 (setq evals-to-be-sent-over (nreverse evals-to-be-sent-over)) ;do in order specified
 (storein-q-value-cell 'sym:lisp-crash-list
		       ;; This MAKE-Q-LIST must not use the FASL-TEMP-AREA,
		       ;; because the list structure being created includes
		       ;; definitions of important macros.
		       (make-q-list 'sym:init-list-area evals-to-be-sent-over))
 ;;Everything compiled, etc. close off and write it out
 (format t "~&q-storage-finalize...")
 (q-storage-finalize))

;nil and t must be stored manually since qnil and qtruth would not be bound when needed
(defun make-t-and-nil ()
  (setq qnil (vmake-pointer sym:dtp-symbol
		      (allocate-block 'sym:resident-symbol-area sym:length-of-atom-head)))
  (vwrite-cdr qnil sym:cdr-next (vmake-pointer sym:dtp-symbol-header
					       (store-string 'sym:p-n-string "NIL")))
  (vwrite-cdr (+ qnil 1) sym:cdr-next qnil)
  (vwrite-cdr (+ qnil 2) sym:cdr-next (vmake-pointer sym:dtp-null qnil))
  (vwrite-cdr (+ qnil 3) sym:cdr-next qnil)
  (vwrite-cdr (+ qnil 4) sym:cdr-next qnil)
  (setf (get 'sym:nil 'q-atom-head) qnil)
  ;(setf (symbol-value 'sym:nil) nil)
  (setq qtruth (vmake-pointer sym:dtp-symbol
		   (allocate-block 'sym:resident-symbol-area sym:length-of-atom-head)))
  (vwrite-cdr qtruth sym:cdr-next (vmake-pointer sym:dtp-symbol-header
						 (store-string 'sym:p-n-string "T")))
  (vwrite-cdr (+ qtruth 1) sym:cdr-next qtruth)
  (vwrite-cdr (+ qtruth 2) sym:cdr-next (vmake-pointer sym:dtp-null qtruth))
  (vwrite-cdr (+ qtruth 3) sym:cdr-next qnil)
  (vwrite-cdr (+ qtruth 4) sym:cdr-next qnil)
  ;(setf (symbol-value 'sym:t) t)
  (setf (get 'sym:t 'q-atom-head) qtruth))

;Fix the values of certain variables before they are sent over
(defun fix-certain-variables ()
  (setq sym:prin1 nil)
  (setq sym:base (setq sym:ibase 10.))
  (setq sym:*print-base* (setq sym:*read-base* 10.))
  (setq sym:*nopoint t)
  (setq sym:for-cadr t)				;Is this still used?
  )

;;; Initializations of all sorts of random variables.  Must follow the map
;;; over q-corresponding-variable-lists, because previous initializations are stored over.
(defun init-random-variables ()
  ;;set up array-types symbol (both value and function cells).
  ;;  the function cell is an array which gives maps numeric array type to symbolic name.
  ;;  the value cell is a list pointer into the above array, so is an ordered list
  ;;   of the array types.
  (init-q-array 'sym:control-tables 'sym:array-types 2 'sym:art-q-list '(32.) nil nil)
  (store-list-of-atoms 'sym:control-tables sym:array-types)
  (store-nils 'sym:control-tables (- 32. (length sym:array-types)))
  (storein-q-value-cell 'sym:array-types
    (vmake-pointer sym:dtp-list (- (aref area-alloc-pointers
					 (get-area-number 'sym:control-tables))
				   32.)))
  ;;set up the array-elements-per-q array.
  (init-q-array 'sym:control-tables 'sym:array-elements-per-q 2 ;fcn
		'sym:art-q-list '(32.) nil nil)
  (store-list-of-atoms 'sym:control-tables (make-ordered-array-list sym:array-elements-per-q))
  (store-nils 'sym:control-tables (- 32. (length sym:array-types)))
  ;;value cell of array-elements-per-q has assq list, is not same as array.
  ;;set up the array-bits-per-element array, similar
  (init-q-array 'sym:control-tables 'sym:array-bits-per-element 2 ;fcn
		'sym:art-q-list '(32.) nil nil)
  (store-list-of-atoms 'sym:control-tables
		       (make-ordered-array-list sym:array-bits-per-element))
  (store-nils 'sym:control-tables (- 32. (length sym:array-types)))
  ;;set up q-data-types
  (init-q-array 'sym:control-tables 'sym:q-data-types 2 'sym:art-q-list '(32.) nil 
		(list (make-q-list 'sym:init-list-area (length sym:q-data-types))))
  (store-list-of-atoms 'sym:control-tables sym:q-data-types)
  (store-nils 'sym:control-tables (- 32. (length sym:q-data-types)))
  (storein-q-value-cell 'sym:q-data-types
    (vmake-pointer sym:dtp-list (- (aref area-alloc-pointers
					 (get-area-number 'sym:control-tables))
				   32.)))
  ;;Make the arrays which are mapped into A-memory
  (init-q-array 'sym:control-tables 'sym:mouse-cursor-pattern 1
		'sym:art-1b '(32. 32.) t nil)
  (let ((adr (allocate-block 'sym:control-tables 2)))
    (vwrite adr (vfix (+ (cadr (memq 'sym:mouse-cursor-pattern sym:a-memory-array-locations))
			 sym:a-memory-virtual-address)))
    (vwrite (1+ adr) (vfix 1024.)))
  (init-q-array 'sym:control-tables 'sym:mouse-buttons-buffer 1
		'sym:art-q '(32.) t nil)
  (let ((adr (allocate-block 'sym:control-tables 2)))
    (vwrite adr (vfix (+ (cadr (memq 'sym:mouse-buttons-buffer sym:a-memory-array-locations))
			 sym:a-memory-virtual-address)))
    (vwrite (1+ adr) (vfix 32.)))
  (init-q-array 'sym:control-tables 'sym:mouse-x-scale-array 1
		'sym:art-q '(16.) t nil)
  (let ((adr (allocate-block 'sym:control-tables 2)))
    (vwrite adr (vfix (+ (cadr (memq 'sym:mouse-x-scale-array sym:a-memory-array-locations))
			 sym:a-memory-virtual-address)))
    (vwrite (1+ adr) (vfix 16.)))
  (init-q-array 'sym:control-tables 'sym:mouse-y-scale-array 1
		'sym:art-q '(16.) t nil)
  (let ((adr (allocate-block 'sym:control-tables 2)))
    (vwrite adr (vfix (+ (cadr (memq 'sym:mouse-y-scale-array sym:a-memory-array-locations))
			 sym:a-memory-virtual-address)))
    (vwrite (1+ adr) (vfix 16.))))
