; -*- Mode:LISP; Package:COLD ; Base:8; Lowercase:T; Readtable:T -*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **

(in-package :cold)

;Loader of QFASL files into cold-loads

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To compile this:			       ;;;
;;;   (1) Load COLDUT QFASL		       ;;;
;;;   (2) Run (LOAD-PARAMETERS)		       ;;;
;;;   (3) Now you may compile it	       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (special evals-to-be-sent-over
		    last-fasl-eval		;the element of evals-to-be-sent-over created
						;  by the last fasl-op-eval.
		    cold-list-area
		    fef-debugging-info-alist
		    current-function))		;debugging aid

(declaim (special fasl-table fasl-table-fill-pointer fasl-return-flag 
		    fasl-group-bits fasl-group-type fasl-group-length fasl-group-flag
		    q-fasl-group-dispatch m-fasl-group-dispatch fasl-group-dispatch-size
		    file-property-list cold-loaded-file-property-lists
		    cold-loaded-function-property-lists))

(declaim (special qfasl-binary-file fdefine-file-pathname))

;Each function defined has its name pushed on this list.
;Then we send the list over together with the package to make
;the DEFINITIONS property of the file.
(defvar this-file-definitions)

;Q-FASL-xxxx refers to functions which load into the cold load, and
; return a "Q", i.e. a list of data-type and address-expression.
;M-FASL-xxxx refers to functions which load into the current Lisp environment
; (M used to be for Maclisp), and return a Lisp object.
; However M-objects still have all their symbols in the SYM package.
;In the FASL-TABLE, each entry in both the prefix and the main part
; is a list whose car is the M (lisp) value and whose cadr is either
; NIL or the Q-value.  If it needs a Q-value and one hasn't been
; computed yet, it will compute one, but this may put it in the wrong area.

;These functions are used to refer to the FASL-TABLE

;Get a Q object from FASL table
(defun q-arft (x)
  (cond ((atom (setq x (aref fasl-table x)))
	 (error "not a q - q-arft"))
	((cadr x))
	(t (rplaca (cdr x) (make-q-list 'sym:init-list-area (car x)))
	   (cadr x))))

;Get an M object
(defun m-arft (x)
  (cond ((atom (setq x (aref fasl-table x)))
	 (error "not a q - m-arft"))
	(t (car x))))

;Store an M object
(defmacro m-asft (d x)
  `(setf (aref fasl-table ,x) (list ,d nil)))

;Store both M and Q objects
(defmacro m-q-asft (d q x)
  `(setf (aref fasl-table ,x) (list ,d ,q)))

(defun cold-fasload (file)
  (or (boundp 'q-fasl-group-dispatch) (initialize-fasl-environment))
  (setq file (logical-pathname file))
  (format t "~&Cold-fasload ~A~%" (namestring file))
  (with-open-file (qfasl-binary-file file :direction :input
				     :element-type '(unsigned-byte 16))
    (initialize-file-plist file)
    (setq fdefine-file-pathname (store-string 'sym:p-n-string
					      (namestring (translate-logical-pathname file))))
    (or (and (= (qfasl-nibble) #o143150)
	     (= (qfasl-nibble) #o71660))
	(error "~A is not a QFASL file" (namestring file)))
    (let (this-file-definitions)
      (do () ((eq (qfasl-whack) 'eof)))
      (set-file-loaded-id file)
      ;; restore this for sys99
      ;(record-definitions this-file-definitions)
      )))

;Add the list of function specs defined in a file
;to that file's property list.  The argument is a list in this world.
(defun record-definitions (definitions)
  (vstore-contents (1+ file-property-list)
		   (vlist* 'sym:property-list-area
			   (qintern 'sym:definitions)
			   ;;(list (cons package definitions))
			   (vlist 'sym:property-list-area
				  (vlist* 'sym:property-list-area
					  qnil
					  (make-q-list 'sym:property-list-area definitions)))
			   (vread (1+ file-property-list)))))

;Initialize an element of *cold-loaded-file-property-lists* for this file.
;The value of *cold-loaded-file-property-lists* is a list of elements:
;	(filenamestring . generic-file-plist)
(defun initialize-file-plist (pathname)
  (cond ((not (boundp 'cold-loaded-file-property-lists))
	 (setq cold-loaded-file-property-lists
	       (qintern 'sym:*cold-loaded-file-property-lists*))
	 (vwrite (+ cold-loaded-file-property-lists 1) qnil)))
  (setq file-property-list (vlist* 'sym:property-list-area
				   (store-string 'sym:p-n-string (file-namestring pathname))
				   qnil))
  (vstore-contents (+ cold-loaded-file-property-lists 1)
		   (vlist* 'sym:property-list-area
			   file-property-list
			   (vread (+ cold-loaded-file-property-lists 1)))))

;This remembers where the file that we are building comes from
(defun set-file-loaded-id (file &aux qid)
  (setq qid (vlist* 'sym:property-list-area
		    (store-string 'sym:p-n-string
				  (file-namestring file))
		    (store-string 'sym:p-n-string
				  (multiple-value-bind (s m h d mo y)
						       (decode-universal-time (file-write-date file))
							
				   (format nil "~D/~D/~D ~D:~D:~D" mo d y h m s)))))
  ;; ((nil fileversionid "coldloaded"))
  (let ((id-prop (vlist 'sym:property-list-area
			(vlist 'sym:property-list-area
			       qnil qid (store-string 'sym:p-n-string "COLDLOADED")))))
    (let ((plist (vlist* 'sym:property-list-area
			 (qintern 'sym:file-id-package-alist)
			 id-prop
			 (vread (1+ file-property-list)))))
      ;; plist = (file-id-package-alist ((nil fileversionid)))
      ;; The nil will get replaced with the SI package later
      (vstore-contents (+ file-property-list 1) plist))))

;This is the function which gets a 16-bit "nibble" from the fasl file.
(defun qfasl-nibble ()
  (read-byte qfasl-binary-file))

;This function processes one "whack" (independent section) of a fasl file.
(defun qfasl-whack ()
  (let ((fasl-table-fill-pointer sym:fasl-table-working-offset)
	(fasl-return-flag nil))
    (or (boundp 'fasl-table)
	(setq fasl-table (make-array sym:length-of-fasl-table
				     :initial-element nil)))
    (initialize-qfasl-table)
    (do () (fasl-return-flag)
      (qfasl-group nil))
    fasl-return-flag))

;Initialize FASL-TABLE prefix
(defun initialize-qfasl-table ()
  (setf (aref fasl-table sym:fasl-symbol-head-area) '(sym:nr-sym nil))
  (setf (aref fasl-table sym:fasl-symbol-string-area) '(sym:p-n-string nil))
  (setf (aref fasl-table sym:fasl-array-area)
	'(sym:control-tables nil))	;I GUESS
  (setf (aref fasl-table sym:fasl-frame-area)
	'(sym:macro-compiled-program nil))
  (setf (aref fasl-table sym:fasl-list-area)
	'(sym:init-list-area nil))	;Not FASL-CONSTANTS-AREA!!
  (setf (aref fasl-table sym:fasl-temp-list-area)
	'(sym:fasl-temp-area nil)))

(defun initialize-fasl-environment ()
  (setf (logical-pathname-translations "sys")
	'(("SYS:**;*.*.*" "/u12/lisp/mit/work/**/*.*")))
  (setq fef-debugging-info-alist nil)
  (setq fasl-group-dispatch-size (length sym:fasl-ops))
  (setq q-fasl-group-dispatch (make-array fasl-group-dispatch-size))
  (setq m-fasl-group-dispatch (make-array fasl-group-dispatch-size))
  (do ((i 0 (1+ i))
       (l sym:fasl-ops (cdr l))
       (*package* (find-package "COLD"))
       (m-op) (q-op))
      ((= i fasl-group-dispatch-size))
    (setq m-op (intern (format nil "M-~A" (car l)))
	  q-op (intern (format nil "Q-~A" (car l))))
    (setf (aref m-fasl-group-dispatch i) m-op)
    (setf (aref q-fasl-group-dispatch i) q-op)))

;Process one "group" (a single operation)
;Argument is NIL for Q-FASL, T for M-FASL.
(defun qfasl-group (m-p &aux fasl-group-flag fasl-group-bits
			     fasl-group-type fasl-group-length)
  (setq fasl-group-bits (qfasl-nibble))
  (or (bit-test sym:%fasl-group-check fasl-group-bits)
      (error "fasl-group-nibble-without-check-bit"))
  (setq fasl-group-flag (bit-test sym:%fasl-group-flag fasl-group-bits)
	fasl-group-length (lispm-ldb sym:%%fasl-group-length fasl-group-bits))
  (and (= fasl-group-length #o377)
       (setq fasl-group-length (qfasl-nibble)))
  (setq fasl-group-type (logand sym:%fasl-group-type fasl-group-bits))
  (or (< fasl-group-type fasl-group-dispatch-size)
      (error "~O erroneous fasl group type" fasl-group-type))
  (funcall (aref (if m-p m-fasl-group-dispatch q-fasl-group-dispatch) fasl-group-type)))

;Get next nibble out of current group
(defun qfasl-next-nibble ()
  (cond ((zerop fasl-group-length) (error "fasl-group-overflow"))
	(t (setq fasl-group-length (1- fasl-group-length))
	   (qfasl-nibble))))

;Get next value for current group.  Works by recursively evaluating a group.
;This one gets a Q value
(defun q-fasl-next-value ()
  (q-arft (qfasl-group nil)))

;This one gets an M value
(defun m-fasl-next-value ()
  (m-arft (qfasl-group t)))

;This one gets both
(defun m-q-fasl-next-value ()
  (let ((idx (qfasl-group nil)))
    (values (m-arft idx) (q-arft idx))))

;FASL-OP's that create a value end up by calling this.  The value is saved
;away in the FASL-TABLE for later use, and the index is returned (as the 
;result of QFASL-GROUP).
;This one enters an M object and a Q
(defun m-q-enter-fasl-table (m q)
  (cond ((not (< fasl-table-fill-pointer sym:length-of-fasl-table))
	 (error "fasl table overflow"))
	(t
	 (m-q-asft m q fasl-table-fill-pointer)
	 (prog1 fasl-table-fill-pointer
		(setq fasl-table-fill-pointer (1+ fasl-table-fill-pointer))))))

;This one enters an M value
(defun m-enter-fasl-table (v)
  (cond ((not (< fasl-table-fill-pointer sym:length-of-fasl-table))
	 (error "fasl table overflow"))
	(t
	 (m-asft v fasl-table-fill-pointer)
	 (prog1 fasl-table-fill-pointer
		(setq fasl-table-fill-pointer (1+ fasl-table-fill-pointer))))))

(defun m-q-store-evaled-value (m q)
  (m-q-asft m q sym:fasl-evaled-value)
  sym:fasl-evaled-value)


;;;; --M-FASL ops

(defun m-fasl-op-noop () 0)

(defun m-fasl-op-index () (qfasl-next-nibble))

(defun m-fasl-op-string ()
  (m-enter-fasl-table (m-fasl-pname)))

(defun m-fasl-op-symbol ()
  (m-enter-fasl-table (cond (fasl-group-flag (make-symbol (m-fasl-pname)))
			    (t (intern (m-fasl-pname) sym-package)))))

(defun m-fasl-op-package-symbol ()
  (do ((i 0 (1+ i))
       (path nil)
       (sym)
       (len (qfasl-next-nibble)))
      ((= i len)
       (setq path (nreverse path))
       (cond ((> (length path) 2)
	      (error "Package path ~S has more than one prefix, I can't handle this"
			  path))
	     ((memq (car path)	;don't get faked out into splitting one symbol into two.
		    '(sym:si sym:system-internals sym:system sym:sys sym:global))
	      (m-enter-fasl-table (cadr path)))
	     (t
	      (setq sym (intern (format nil "~{~A~^:~}" path) sym-package))
	      (setf (get sym 'package-path) path)
	      (m-enter-fasl-table sym))))
      (push (intern (m-fasl-next-value) sym-package) path)))  ;fasl-value is string

(defun m-fasl-pname ()				;Return a string
  (let ((len (* fasl-group-length 2))
	lst lst2
	str
	tem)
    (dotimes (i fasl-group-length)
      (setq tem (qfasl-next-nibble))
      (push (code-char (ldb (byte 8 0) tem)) lst)
      (setq tem (ash tem -8))
      (if (eq tem #o200)
	  (setq len (1- len))
	(push (code-char tem) lst)))
    (setq lst2 (nreverse lst))
    (setq str (make-string len))
    (dotimes (i len)
	     (setf (aref str i) (car lst2))
	     (setq lst2 (cdr lst2)))
    str))

;Generate a FIXNUM (or BIGNUM) value.
(defun m-fasl-op-fixed ()
  (do ((pos (* (1- fasl-group-length) #o20) (- pos #o20))
       (c fasl-group-length (1- c))
       (ans 0))
      ((zerop c) (cond (fasl-group-flag (setq ans (- 0 ans))))
		 (m-enter-fasl-table ans))
    (setq ans (lispm-dpb (qfasl-next-nibble) (+ (ash pos 6) #o20) ans))))

;Generate a CHARACTER value.
(defun m-fasl-op-character ()
  (do ((pos (* (1- fasl-group-length) #o20) (- pos #o20))
       (c fasl-group-length (1- c))
       (ans 0))
      ((zerop c)
       (cond (fasl-group-flag (setq ans (- 0 ans))))
       ;; comment out next line if building earlier than system 99
       (setq ans (%make-pointer sym:dtp-character ans))
       (m-enter-fasl-table ans))
      (setq ans (lispm-dpb (qfasl-next-nibble) (+ (ash pos 6) #o20) ans))))

;;; NEW FLOAT OP!! not yet written. See sys; qfasl

(defun m-fasl-op-float ()
  (q-fasl-op-float))

;;; XXX fix this
(defun m-fasl-op-float-float ()
  (prog (ans tmp)
    (setq ans (float 0))
    ;(%p-dpb-offset (qfasl-next-nibble) #o1013 ans 0)
    (qfasl-next-nibble)
    (setq tmp (qfasl-next-nibble))
    ;(%p-dpb-offset (lispm-ldb #o1010 tmp) #o0010 ans 0)
    ;(%p-dpb-offset (lispm-dpb tmp #o2010 (qfasl-next-nibble)) #o0030 ans 1)
    (qfasl-next-nibble)
    (return (m-enter-fasl-table ans))))


(defun m-fasl-op-list () (q-fasl-op-list))

(defun m-fasl-op-temp-list () (m-fasl-op-list1))

(defun m-fasl-op-list-component () (q-fasl-op-list t))

(defun m-fasl-op-list1 ()
  (do ((list-length (qfasl-next-nibble) (1- list-length))
       (lst nil) (adr) (tem))
      ((zerop list-length)
       (m-q-enter-fasl-table lst '**screw**))
    (cond ((and fasl-group-flag (= list-length 1)) ;dotted
	   (rplacd adr (m-fasl-next-value)))
	  (t (setq tem (cons (m-fasl-next-value) nil))
	     (and adr (rplacd adr tem))
	     (or lst (setq lst tem))
	     (setq adr tem)))))

;;;; --Q-FASL ops

(defun q-fasl-op-noop () 0)

(defun q-fasl-op-index () (qfasl-next-nibble))

(defun q-fasl-op-string ()
  (let ((str (m-fasl-pname)))
    (m-q-enter-fasl-table str (store-string 'sym:p-n-string str))))

(defun q-fasl-op-package-symbol ()
  (let ((x (m-fasl-op-package-symbol)))
    (q-arft x)
    x))

(defun q-fasl-op-symbol ()
  (let ((sym (m-fasl-pname)))
    (m-q-enter-fasl-table (if fasl-group-flag (make-symbol sym)
			      (setq sym (intern sym sym-package)))
			  (if fasl-group-flag ;uninterned
			      (store-symbol-vector sym 'sym:nr-sym)
			      (qintern sym)))))

(defun q-fasl-op-fixed ()
  (let ((x (m-fasl-op-fixed)))
    (q-arft x)
    x))

(defun q-fasl-op-character ()
  (let ((x (m-fasl-op-character)))
    (q-arft x)
    x))

(defun q-fasl-op-float ()
  (cond (fasl-group-flag (q-fasl-op-small-float))
	(t (q-fasl-op-float-float))))

(defun q-fasl-op-small-float ()
  (let ((as-fixnum (lispm-dpb (qfasl-next-nibble) #o2010 (qfasl-next-nibble))))
    ;; When running in systems after 98, we will want to
    ;; change exponent from excess #o100 to excess #o200.
;    (setq as-fixnum (if (zerop as-fixnum) 0 (%pointer-plus as-fixnum #o40000000)))
    (let ((num (%make-pointer sym:dtp-small-flonum as-fixnum)))
      (m-q-enter-fasl-table num (make-small-flonum num)))))

(defun q-fasl-op-float-float ()
  (let ((x (m-fasl-op-float-float)))
    (q-arft x)
    x))


;;; Total kludgery.  FASL-OP-TEMP-LIST makes an M list, assumed to be
;;; going to get fed to something like FASL-OP-ARRAY or FASL-OP-EVAL.
;;; FASL-OP-LIST, on the other hand, makes a Q list, assumed to
;;; be going to be used for something like a macro.  In either case the
;;; area specification in the FASL table is ignored.
;;; Hopefully this kludgery stands some chance of working.

(defun q-fasl-op-temp-list ()
  (m-fasl-op-list))
       
(defun q-fasl-op-list-component ()
  (q-fasl-op-list t))

(defun q-fasl-op-list (&optional component-flag)
  (let ((area cold-list-area)
	(list-length (qfasl-next-nibble))
	lst c-code maclisp-list fasl-idx)
    (or (memq area sym:list-structured-areas)
	(error "q-fasl-op-list in non-list-structured area"))
    (setq lst (allocate-block area list-length))
    (do ((adr lst (1+ adr))
	 (len list-length (1- len)))
	((zerop len))
      (setq c-code (cond ((and fasl-group-flag (= len 2)) sym:cdr-normal)
			 ((and fasl-group-flag (= len 1)) sym:cdr-error)
			 ((= len 1) sym:cdr-nil)
			 (t sym:cdr-next)))
      (setq fasl-idx (qfasl-group nil))
      (vwrite-cdr adr c-code (q-arft fasl-idx))
      (setq maclisp-list (nconc maclisp-list
				(if (and fasl-group-flag (= len 1)) (m-arft fasl-idx)
				    (cons (m-arft fasl-idx) nil)))))
    (if (null component-flag)
	(m-q-enter-fasl-table maclisp-list (vmake-pointer sym:dtp-list lst))
	(m-q-store-evaled-value maclisp-list (vmake-pointer sym:dtp-list lst)))))

;;;; Array stuff

(defvar last-array-dims)
(defvar last-array-type)

;FASL-OP-ARRAY arguments are
; <value>  Area 
; <value>  Type symbol
; <value>  The dimension or dimension list (use temp-list)
; <value>  Displace pointer (NIL if none)
; <value>  Leader (NIL, number, or list) (use temp-list)
; <value>  Index offset (NIL if none)
; <value>  Named-structure (only present if flag bit set)
(defun q-fasl-op-array ()
  (let* ((flag fasl-group-flag)
	(area (m-fasl-next-value))
	(type-sym (m-fasl-next-value))
	(dims (m-fasl-next-value))
	(displaced-p (m-fasl-next-value))  ;if non-nil, will it work?
	(leader (m-fasl-next-value))
	(index-offset (m-fasl-next-value)) ;if non-nil, will it work?
	(named-structure nil)
	(array nil) (data-length nil) (adr nil))
     (setq area 'sym:control-tables) ;kludge, may not be needed any more
     (cond (flag
	    (setq named-structure (m-fasl-next-value))))
     (and (not (atom leader))
	  (setq leader (mapcar (function (lambda (x) (make-q-list 'sym:init-list-area x)))
			       leader)))
     (setq last-array-dims (if (numberp dims) (list dims) dims))
     (setq last-array-type type-sym)
     (setq array (init-q-array-named-str area
					 nil  ;return list of address and data-length
					 index-offset
					 type-sym
					 dims
					 displaced-p
					 leader
					 named-structure))
     (setq data-length (cadr array)
	   array (vmake-pointer sym:dtp-array-pointer (car array)))
     ;now store the data area
     (and displaced-p (error "displaced array not handled"))
     (setq adr (allocate-block area data-length))
     (cond ((cdr (assoc type-sym sym:array-bits-per-element :test #'eq)) ;numeric
	    (dotimes (i data-length)
	      (vwrite (+ adr i) 0)))
	   (t
	    (cond ((and named-structure (not leader))
		   (vwrite adr (qintern named-structure))
		   (setq adr (1+ adr)
			 data-length (1- data-length))))
	    (dotimes (i data-length)
	      (vwrite (+ adr i) qnil))))
     (m-q-enter-fasl-table
       "note - you have been screwed to the wall by an array"
       array)))

;Get values and store them into an array.
(defun q-fasl-op-initialize-array ()
  (prog (array num hack ptr header long-flag ndims)
     (setq hack (qfasl-group nil))
     (setq array (q-arft hack))
     (or (= (vdata-type array) sym:dtp-array-pointer)
	 (error "fasl-op-initialize-array of non-array"))
     (setq num (m-fasl-next-value))	;number of values to initialize with
     ;; Take header apart to find address of data
     (setq ptr (logand q-pointer-mask array))
     (setq header (vread ptr))
     (setq long-flag (bit-test sym:array-long-length-flag header)
	   ndims (logand (rem header sym:array-dim-mult) 7))
     (and (bit-test sym:array-displaced-bit header)
	  (error "attempt to initialize displaced array, give it up"))
     (unless (<= 1 (length last-array-dims) 2)
       (error "Only 1 and 2-dimensional arrays can be loaded."))
     (setq ptr (+ ptr (if long-flag 1 0) ndims))	;To data
     ;; Order of data matches order in world being created, so it's easy.
     (dotimes (n num)				;Initialize specified num of vals
	      (vwrite ptr (q-fasl-next-value))
	      (setq ptr (1+ ptr)))
       ;; XXX
;       (let ((temp1 (make-array last-array-dims :initial-element qnil)) temp2)
;	 ;; Read in the values, then transpose them,
;	 (dotimes (n num)
;	   (setf (ar-1-force temp1 n) (q-fasl-next-value)))
;	 (setq temp2 (math:transpose-matrix temp1))
	 ;; Then write them into the cold load in their new order.
;	 (dotimes (n (array-length temp2))
;	   (vwrite ptr (ar-1-force temp2 n))
;	   (setq ptr (1+ ptr)))))
     (return hack)))

;Get 16-bit nibbles and store them into an array.
(defun q-fasl-op-initialize-numeric-array ()
  (prog (array num hack ptr header long-flag ndims)
     (setq hack (qfasl-group nil))
     (setq array (q-arft hack))
     (or (= (vdata-type array) sym:dtp-array-pointer)
	 (error "fasl-op-initialize-array of non-array"))
     (setq num (m-fasl-next-value))	;number of values to initialize with
     ;; Take header apart to find address of data
     (setq ptr (logand q-pointer-mask array))
     (setq header (vread ptr))
     (setq long-flag (bit-test sym:array-long-length-flag header)
	   ndims (lispm-ldb sym:%%array-number-dimensions header))
     (and (bit-test sym:array-displaced-bit header)
	  (error "attempt to initialize displaced array, give it up"))
     (setq ptr (+ ptr (if long-flag 1 0) ndims))	;To data
     (unless (or (= (length last-array-dims) 1)
		 (and (= (length last-array-dims) 2)
		      (eq last-array-type 'sym:art-16b)))
       (error "Only 1-dimensional, or 2-dimensional art-16b, numeric arrays can be loaded."))

     ;; Order of data matches order in world being created, so it's easy.
     (progn
       (dotimes (n (/ num 2))	;Initialize specified num of vals
		(vwrite ptr (+ (qfasl-nibble) (ash (qfasl-nibble) 16.)))
		(setq ptr (1+ ptr)))
       (cond ((oddp num)				;odd, catch last nibble
	      (vwrite ptr (qfasl-nibble)))))
     ;; XXX
;       (let ((temp1 (make-array last-array-dims ':type art-16b)) temp2)
	 ;; Read in the values, then transpose them,
;	 (dotimes (n num)	;Initialize specified num of vals
;	   (setf (ar-1-force temp1 n) (qfasl-nibble)))
;	 (setq temp2 (math:transpose-matrix temp1))
	 ;; Then write them into the cold load in their new order.
;	 (dotimes (n (floor (array-length temp2) 2))
;	   (vwrite ptr (lispm-dpb (ar-1-force temp2 (+ n n 1)) 2020 (ar-1-force temp2 (+ n n))))
;	   (incf ptr))
;	 (if (oddp (array-length temp2))
;	     (vwrite ptr (ar-1-force temp2 (1- (array-length temp2)))))))
     (return hack)))

(defun q-fasl-op-eval ()
  (error "FASL-OP-EVAL isn't supposed to be used any more."))

;(defun q-fasl-op-eval ()
;  (let ((exp (m-arft (qfasl-next-nibble))))
;    (cond ((and (not (atom exp))
;		(eq (car exp) 'sym:record-source-file-name)
;		(not (atom (cadr exp)))
;		(eq (caadr exp) 'sym:quote)
;		(symbolp (cadadr exp)))
;	   (store-source-file-name-property (qintern (cadadr exp))))
;	  (t ;; If this is a defvar or defconst, store the value now
;	     ;; in addition to causing it to be evaluated later.
;	     ;; The evaluation later sets appropriate properties,
;	     ;; while storing the value now prevents lossage
;	     ;; if the value is used while performing the initialization.
;	     (and (not (atom exp))
;		  (memq (car exp) '(sym:defvar-1 sym:defconst-1))
;		  (cddr exp)   ;Only if a value is specified!
;		  (or (memq (caddr exp) '(sym:t sym:nil))
;		      (stringp (caddr exp)) (numberp (caddr exp))
;		      (quotep (caddr exp)))
;		  (progn 
;		    (vstore-contents (1+ (qintern (cadr exp)))
;				     (make-q-list 'sym:init-list-area
;						  (if (quotep (caddr exp))
;						      (cadr (caddr exp))
;						    (caddr exp))))))
;	     (setq evals-to-be-sent-over
;		   (setq last-fasl-eval (cons exp evals-to-be-sent-over))))))
;  (m-q-store-evaled-value 'value-only-available-in-the-future
;			  'value-only-available-in-the-future))

(defun quotep (exp)
  (and (consp exp) (eq (car exp) 'sym:quote)))

(defun q-fasl-op-move ()
  (let* ((from (qfasl-next-nibble))
	(to (qfasl-next-nibble)))
    (cond ((= to #o177777) (m-q-enter-fasl-table (car (aref fasl-table from))
					       (cadr (aref fasl-table from))))
	  (t (setf (aref fasl-table to) (aref fasl-table from))
	     to))))

;;;; Macrocompiled code

(defun q-fasl-op-frame ()
  (let* ((q-count (qfasl-next-nibble))		;number of boxed qs
	(unboxed-count (qfasl-next-nibble))	;number of unboxed qs (half num instructions)
	(fef)					;the fef being created
	(obj)
	(m-obj)
	(fname)
	(tem)
	(offset 0)
	(area 'sym:macro-compiled-program))	;(m-arft sym:fasl-frame-area)
     (setq fasl-group-length (qfasl-next-nibble))	;amount of stuff that follows
     (setq fef (vmake-pointer sym:dtp-fef-pointer	;Store header
			      (storeq area (vmake-pointer sym:dtp-header
							  (m-fasl-next-value)))))
     (qfasl-next-nibble)			;skip modifier nibble for header q
     (do ((i 1 (1+ i))) ((>= i q-count))		;fill in boxed qs
       (multiple-value-setq (m-obj obj) (m-q-fasl-next-value))	;get object to be stored
       (setq tem (qfasl-next-nibble))		;get ultra-kludgey modifier
       (or (zerop (setq offset (logand #o17 tem)))	;add offset if necessary
	   (setq obj (+ obj offset)))
       (and (bit-test #o420 tem)		;try not to get shafted totally
	    (or (= (vdata-type obj) sym:dtp-symbol)
		(error "about to get shafted totally - q-fasl-op-frame")))
       (and (bit-test #o20 tem)			;make into external value cell pointer
	    (setq obj (vmake-pointer sym:dtp-external-value-cell-pointer obj)))
       (and (bit-test #o400 tem)			;make into locative
	    (setq obj (vmake-pointer sym:dtp-locative obj)))
       (setq obj (lispm-dpb (ash tem -6) sym:%%q-cdr-code obj))
       (storeq area obj)
       (if (= i sym:%fefhi-fctn-name) (setq fname m-obj)))
     (push (cons fname m-obj) fef-debugging-info-alist)
     (begin-store-halfwords area unboxed-count)	;now store the unboxed qs
     (do ((n 0 (1+ n))
	  (num (* 2 unboxed-count)))
	 ((= n num))
       (store-halfword (qfasl-next-nibble)))
     (end-store-halfwords)
     (m-q-enter-fasl-table
        "note - you have been screwed to the wall by a fef"
	fef)))

(defun q-fasl-op-function-header ()
  (prog (f-sxh)
	(setq current-function (m-fasl-next-value)
	      f-sxh (m-fasl-next-value))
	(return 0)))

(defun q-fasl-op-function-end () 0)

(defprop sym:|:INTERNAL| (keyword internal) package-path)
(defprop sym:|:PROPERTY| (keyword property) package-path)
(defprop sym:|:INTERNAL-FEF-OFFSETS| (keyword internal-fef-offsets) package-path)
(defprop sym:|:SOURCE-FILE-NAME| (keyword source-file-name) package-path)

(defun q-fasl-storein-symbol-cell (n put-source-file-name-property)
  (prog (newp adr data sym nib)
     (setq nib (qfasl-next-nibble))
     (setq sym (m-fasl-next-value))
     (and put-source-file-name-property
	  (or (atom sym) (not (eq (car sym) 'sym:|:INTERNAL|)))
	  (store-source-file-name-property sym (if (= n 1) 'defvar 'defun)))
     (and (cond ((= nib sym:fasl-evaled-value)
		 ;; From fasl-op-eval
		 (or (setq data last-fasl-eval)
		     (error "~S invalid storein-symbol" sym))
		 t)
		;; From fasl-op-eval1
		((listp (setq data (q-arft nib)))))
	  ;; Setting symbol to result of some evaluation
	  (cond ((atom sym)	          ;Modify the entry in EVALS-TO-BE-SENT-OVER
		 (rplaca data
			 `(,(case n
			      (1 'set)
			      (2 'fset)
			      (otherwise
			       (error
			       "Result of evaluation must be stored in value or function cell"
				       )))
			   (sym:quote ,sym) ,(car data)))
		 (return 0))			;Skip the rest of this function
		(t (error "Must be a sym evaled-value"))))
     (cond ((atom sym)
	    (setq sym (qintern sym))
	    (vstore-contents (+ sym n) data))
	   ((eq (car sym) 'sym:|:INTERNAL|)
	    (or (= n 2) (error "~S only allowed for function cell" sym))
	    (let* ((parent (cadr sym))
		   (index (caddr sym))
		   (table (cdr (assoc 'sym:|:INTERNAL-FEF-OFFSETS|
				     (cdr (assoc parent fef-debugging-info-alist))
				     :test #'eq)))
		   (fef (qfdefinition parent)))
	      (or table (error "Cannot locate internal-fef-offsets for ~S" sym))
	      (or (= (lispm-ldb sym:%%q-data-type fef) sym:dtp-fef-pointer)
		  (error "~S not fef as function definition of ~S" fef parent))
	      (vstore-contents (+ fef (nth index table)) data)))
	   ;; E.g. (DEFUN (FOO PROP) (X Y) BODY)
	   ;; - thinks it's storing function cell but really PUTPROP
	   ((not (and (= n 2) (eq (car sym) 'sym:|:PROPERTY|)))
	    (error "~S not a symbol or property spec" sym))
	   (t (setq adr (qintern (cadr sym)))
	      (setq newp (vmake-pointer sym:dtp-list
					(store-cdr-q 'sym:property-list-area sym:cdr-next
						     (qintern (caddr sym)))))
	      (store-cdr-q 'sym:property-list-area sym:cdr-normal data)
	      (store-cdr-q 'sym:property-list-area sym:cdr-error (vread (+ adr 3)))
	      (vstore-contents (+ adr 3) newp)))
     (return 0)))

(defun qfdefinition (sym)
  (cond ((symbolp sym) (vread (+ (qintern sym) 2)))
	((eq (car sym) 'sym:|:INTERNAL|)
	 (let* ((parent (cadr sym))
		(index (caddr sym))
		(table (cdr (assoc 'sym:|:INTERNAL-FEF-OFFSETS|
				  (cdr (assoc parent fef-debugging-info-alist))
				  :test #'eq)))
		(fef (qfdefinition parent)))
	   (or table (error "Cannot locate internal-fef-offsets for ~S" sym))
	   (vread (+ fef (nth index table)))))
	(t (error "~S garbage function spec" sym))))

;The value of cold-load-function-property-lists is a list of elements:
;	(function-spec indicator value)
(defun store-source-file-name-property (sym type)
  (push (cons sym type)
	this-file-definitions)
  (cond ((atom sym)
	 (let* ((sym (qintern sym))
		(old-prop-location (vget-location-or-nil (+ sym 3)
							 (qintern 'sym:|:SOURCE-FILE-NAME|))))
	   (cond ((= old-prop-location qnil)
		  ;; No existing :source-file-name property.  Create one.
		  (let ((lst (if (eq type 'defun)
			      fdefine-file-pathname
			    (vlist 'sym:property-list-area
				   (vlist 'sym:property-list-area (qintern type)
					  fdefine-file-pathname)))))
		 (vstore-contents (+ sym 3)
				  (vlist* 'sym:property-list-area
					  (qintern 'sym:|:SOURCE-FILE-NAME|)
					  lst
					  (vcontents (+ sym 3))))))
		  (t
	     ;; Existing property.  If it is not a list, just a pathname,
	     ;; convert it to a list.
	     (unless (= (vdata-type (vcontents old-prop-location)) sym:dtp-list)
	       (vstore-contents old-prop-location
				(vlist 'sym:property-list-area
				       (vlist 'sym:property-list-area
					      (qintern 'defun)
					      (vcontents old-prop-location)))))
	     ;; Then add a new element to the property value.
	     (vstore-contents old-prop-location
			      (vlist* 'sym:property-list-area
				      (vlist 'sym:property-list-area (qintern type)
					     fdefine-file-pathname)
				      (vcontents old-prop-location)))))))
	(t
	 (cond ((not (boundp 'cold-loaded-function-property-lists))
		(setq cold-loaded-function-property-lists
		      (qintern 'sym:cold-load-function-property-lists))
		(vwrite (+ cold-loaded-function-property-lists 1) qnil)))
	 (let ((elem (vmake-pointer sym:dtp-list
				    (store-cdr-q 'sym:property-list-area sym:cdr-next
						 (make-q-list 'sym:property-list-area sym)))))
	   (store-cdr-q 'sym:property-list-area sym:cdr-next (qintern 'sym:|:SOURCE-FILE-NAME|))
	   (store-cdr-q 'sym:property-list-area sym:cdr-nil fdefine-file-pathname)
	   (let ((newp (vmake-pointer sym:dtp-list
				      (store-cdr-q 'sym:property-list-area sym:cdr-normal
						   elem))))
	     (store-cdr-q 'sym:property-list-area sym:cdr-error
			  (vread (+ cold-loaded-function-property-lists 1)))
	     (vstore-contents (+ cold-loaded-function-property-lists 1) newp))))))

(defun q-fasl-op-storein-symbol-value ()
  (q-fasl-storein-symbol-cell 1 nil))

(defun q-fasl-op-storein-function-cell ()
  (q-fasl-storein-symbol-cell 2 t))

(defun q-fasl-op-storein-property-cell ()
  (q-fasl-storein-symbol-cell 3 nil))

(defun q-fasl-op-storein-array-leader ()
  (let* ((array (q-arft (qfasl-next-nibble)))
	(subscr (m-arft (qfasl-next-nibble)))
	(value (q-arft (qfasl-next-nibble))))
    ;;error checking might be nice
    ;(store-array-leader value array subscr)
    (vwrite (- array (+ 2 subscr)) value)
    0))

(defun q-fasl-fetch-symbol-cell (n)
  (let* ((sym (q-fasl-next-value))
	 (val (vcontents (+ sym n))))
    (if (= (vdata-type val) sym:dtp-null)
	(error "fetch of unbound symbol cell"))
    (m-q-enter-fasl-table "symbol component" val)))

(defun q-fasl-op-fetch-symbol-value ()
  (q-fasl-fetch-symbol-cell 1))

(defun q-fasl-op-fetch-function-cell ()
  (q-fasl-fetch-symbol-cell 2))

(defun q-fasl-op-fetch-property-cell ()
  (q-fasl-fetch-symbol-cell 3))

(defun q-fasl-op-end-of-whack ()
  (setq fasl-return-flag 'end-of-whack)
  0)

(defun q-fasl-op-end-of-file ()
  (setq fasl-return-flag 'eof)
  0)

(defun q-fasl-op-soak ()
  (dotimes (count (qfasl-next-nibble))
    (m-fasl-next-value))
  (qfasl-group t))

(defun q-fasl-op-set-parameter ()
  (let* ((to (m-fasl-next-value))
	(from (qfasl-group t)))
    (setf (aref fasl-table to) (aref fasl-table from))
    0))

(defun q-fasl-op-file-property-list ()
  (vstore-contents (+ file-property-list 1) (q-fasl-next-value))
  0)

;;; Pathnames are dumped out so as to turn into real ones when fasloaded,
;;; fake up a string instead.  fs:canonicalize-cold-load-pathames will fix it back.
(defun q-fasl-op-eval1 (&aux form pathname)
  (setq form (m-fasl-next-value))
  ;(format t "op-eval1: ~S~%" form)
  (cond ((memq (car form) '(sym:|FS:MAKE-PATHNAME-INTERNAL|
			     sym:|FS:MAKE-FASLOAD-PATHNAME|))
	 (or (setq pathname (do ((flist (cdr form) (cdr flist))
				 (plist nil)
				 (elem))
				((null flist)
				 (nreverse plist))
			      (setq elem (car flist))
			      (or (and (listp elem)
				       (eq (car elem) 'sym:quote))
				  (return nil))
			      (setq elem (cadr elem))
			      ;; Fix up host
			      (cond ((and (null plist) (stringp elem)))
				    ((memq elem '(sym:unspecific sym:|:UNSPECIFIC|))
				     (setq elem :unspecific))
				    ((memq elem '(sym:newest sym:|:NEWEST|))
				     (setq elem 1))
				    ((or (null elem) (stringp elem) (numberp elem)))
				    ((listp elem)
				     (if (dolist (elemelem elem)
					   (or (stringp elemelem) (return t)))
					 (return nil)))
				    (t (return nil)))
			      (push elem plist)))
	     (error "This pathname is too complicated for me ~S" form))
	 (setq pathname (cold-pathname pathname))
	 (m-q-enter-fasl-table pathname (store-string 'sym:p-n-string pathname)))
    ((eq (car form) 'sym:record-source-file-name)
     (store-source-file-name-property (cadr (cadr form)) (cadr (caddr form)))
     (m-q-enter-fasl-table nil qnil))
    ;; I have a suspicion the next clause does nothing
    ;; because the compiler does not express DEFF in this way.
    ((and (eq (car form) `sym:deff)
	  (symbolp (cadr (cadr form))))
     (store-source-file-name-property (cadr (cadr form)) 'defun)
     (push form evals-to-be-sent-over)
;     (vstore-contents (+ (qintern function) 2)
;		      (make-q-list 'sym:init-list-area defsym))
;     (unless (= (lispm-ldb sym:%%q-data-type (vcontents (+ (qintern defsym) 2)))
;		sym:dtp-null)
;       (vstore-contents (+ (qintern function) 2) (vcontents (+ (qintern defsym) 2))))
     )
    ((and (eq (car form) 'sym:forward-value-cell) ;(sym:quote ,alias-sym) (sym:quote ,defsym))
     (symbolp (cadr (cadr form))) (symbolp (cadr (caddr form))))
     (push form evals-to-be-sent-over)
     (vstore-contents (+ (qintern (cadr (cadr form))) 1)
		      (lispm-dpb sym:dtp-one-q-forward
			   sym:%%q-data-type
			   (+ (qintern (cadr (caddr form))) 1)))
     (m-q-enter-fasl-table nil qnil))
    (t
     ;; If this is a defvar or defconst, store the value now
     ;; in addition to causing it to be evaluated later.
     ;; The evaluation later sets appropriate properties,
     ;; while storing the value now prevents lossage
     ;; if the value is used while performing the initialization.
     (cond ((and (not (atom form))
		 (memq (car form) '(sym:defvar-1 sym:defconst-1)))
	    (and (cddr form)			;Only if a value is specified!
		 (or (memq (caddr form) '(sym:t sym:nil))
		     (stringp (caddr form)) (numberp (caddr form))
		     (quotep (caddr form)))
		 (vstore-contents (1+ (qintern (cadr form)))
				  (make-q-list 'sym:init-list-area
					       (if (quotep (caddr form))
						   (cadr (caddr form))
						   (caddr form)))))
	    (store-source-file-name-property (cadr form) 'defvar)))
     (push form evals-to-be-sent-over)
     ;; We store a pointer to the position in the evals to be sent over in the
     ;; q fasl table.  q-fasl-storein-symbol-cell knows how to interpret this.
     (m-q-enter-fasl-table 'value-only-available-in-the-future
			   evals-to-be-sent-over))))

