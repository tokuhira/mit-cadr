;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-

;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Loading this with a base of other than 8 can really cause bizarre effects
;GLOBAL:(UNLESS (= *READ-BASE* 8) (BREAK "*READ-BASE* not 8."))

;;; Numeric values of data types, suitable for being DPB'd into the
;;; data type field, or returned by (%DATA-TYPE ...).
(cl:defvar Q-DATA-TYPES '(
  DTP-TRAP
  DTP-NULL
  DTP-FREE 
  DTP-SYMBOL
  DTP-SYMBOL-HEADER
  DTP-FIX
  DTP-EXTENDED-NUMBER
  DTP-HEADER 
  DTP-GC-FORWARD
  DTP-EXTERNAL-VALUE-CELL-POINTER
  DTP-ONE-Q-FORWARD
  DTP-HEADER-FORWARD
  DTP-BODY-FORWARD
  DTP-LOCATIVE
  DTP-LIST 
  DTP-U-ENTRY 
  DTP-FEF-POINTER
  DTP-ARRAY-POINTER
  DTP-ARRAY-HEADER 
  DTP-STACK-GROUP
  DTP-CLOSURE
  DTP-SMALL-FLONUM
  DTP-SELECT-METHOD 
  DTP-INSTANCE
  DTP-INSTANCE-HEADER
  DTP-ENTITY
  DTP-STACK-CLOSURE
  DTP-SELF-REF-POINTER
  DTP-CHARACTER))

;;; Numeric values of CDR codes, right-justified in word for %P-CDR-CODE, etc.
(cl:defvar Q-CDR-CODES '(
  CDR-NORMAL
  CDR-ERROR
  CDR-NIL
  CDR-NEXT))

;;; Byte pointers at the parts of a Q or other thing, and their values.
;;; Q-FIELD-VALUES does NOT itself go into the cold load.
(cl:defvar Q-FIELD-VALUES '(
  %%Q-CDR-CODE 3602
  %%Q-BOXED-SIGN-BIT 3001
  %%Q-DATA-TYPE 3105
  %%Q-POINTER 0031
  %%Q-POINTER-WITHIN-PAGE 0010
  %%Q-TYPED-POINTER 0036
  %%Q-ALL-BUT-TYPED-POINTER 3602
  %%Q-ALL-BUT-POINTER 3107
  %%Q-ALL-BUT-CDR-CODE 0036
  %%Q-HIGH-HALF 2020				;Use these for referencing macro instructions
  %%Q-LOW-HALF 0020
  %%CH-FONT 1010
  %%CH-CHAR 0010
  %%KBD-CHAR 0010
  %%KBD-CONTROL-META 2504
  %%KBD-CONTROL 2501
  %%KBD-META 2601
  %%KBD-SUPER 2701
  %%KBD-HYPER 3001
  %%KBD-MOUSE 2401
  %%KBD-MOUSE-BUTTON 0003
  %%KBD-MOUSE-N-CLICKS 0303
  %%BYTE-SPECIFIER-POSITION 0627
  %%BYTE-SPECIFIER-SIZE 0006))

;;; Assign the byte pointers their values. Q-FIELDS becomes a list of just names.
;;; It goes into the cold load, along with the names and their values.
(ASSIGN-ALTERNATE Q-FIELD-VALUES)
(cl:defvar Q-FIELDS (GET-ALTERNATE Q-FIELD-VALUES))

;;; Stuff in the REGION-BITS array, some of these bits also appear in the
;;; map in the same orientation.  

(cl:defvar Q-REGION-BITS-VALUES '(
  %%REGION-MAP-BITS 1612		;10 bits to go into the map (access/status/meta)
  ;; 2404				;access and status bits
  %%REGION-OLDSPACE-META-BIT	2301	;0=old or free, 1=new or static or fixed.
					;0 causes transport-trap for read of ptr to here
  %%REGION-EXTRA-PDL-META-BIT	2201	;0=extra-pdl, 1=normal.
					;0 traps writing of ptr to here into "random" mem
  %%REGION-REPRESENTATION-TYPE	2002		;Data representation type code:
  %REGION-REPRESENTATION-TYPE-LIST	0
  %REGION-REPRESENTATION-TYPE-STRUCTURE	1	;2 and 3 reserved for future
  ;; 1602 spare meta bits
  ;; 1501 spare (formerly unimplemented compact-cons flag)
  %%REGION-SPACE-TYPE		1104		;Code for type of space:
  %REGION-SPACE-FREE		0		;0 free region slot
  %REGION-SPACE-OLD		1		;1 oldspace region of dynamic area
  %REGION-SPACE-NEW		2		;2 permanent newspace region of dynamic area
  %REGION-SPACE-NEW1		3		;3 temporary space, level 1
  %REGION-SPACE-NEW2		4		;4 ..
  %REGION-SPACE-NEW3		5		;5 ..
  %REGION-SPACE-NEW4		6		;6 ..
  %REGION-SPACE-NEW5		7		;7 ..
  %REGION-SPACE-NEW6		10		;10 ..
  %REGION-SPACE-STATIC		11		;11 static area
  %REGION-SPACE-FIXED		12		;12 fixed, static
						;          not growable
						;          no consing allowed
  %REGION-SPACE-EXTRA-PDL	13		;13 An extra-pdl for some stack-group
  %REGION-SPACE-COPY		14		;14 Like newspace, stuff copied from oldspace
						;    goes here while newly-consed stuff goes
						;    to newspace. This is for permanent data
						;15-17 [not used]

  %%REGION-SCAVENGE-ENABLE	1001		;If 1, scavenger touches this region
  ;; 0503 spare bits.
  %%REGION-SWAPIN-QUANTUM	0005		;swap this +1 pages in one disk op on swapin
						;  if possible.
  ))

(ASSIGN-ALTERNATE Q-REGION-BITS-VALUES)
(cl:defvar Q-REGION-BITS (GET-ALTERNATE Q-REGION-BITS-VALUES))

(cl:defvar SYSTEM-COMMUNICATION-AREA-QS '(
  ;; LOCATIONS RELATIVE TO 400 IN CADR
  ;; locations 400-437 are miscellaneous Qs declared below
  ;; locations 440-477 are the reverse first level map
  ;; locations 500-511 are the keyboard buffer header (buffer is 200-377)
  ;; locations 600-637 are the disk-error log
  ;; locations 700-777 are reserved for disk CCW's (only 777 used now)
  ;; In CADR, location 777 is used (for now) by the disk code for the CCW.
  ;;  --actually it seems to use locations 12-377 for the CCW most of the time.
  ;;  THE FOLLOWING ARE COMMENTS FOR THE LAMBDA
  ;; locations 640-647 are the IOPB command block for disk control on Lambda.
  ;; locations 650-667 are the PHYSICAL order to NUBUS-SLOT map on Lambda.
  ;;   ea word applies to next memory in sequence, bit 31 set terminates list.
  ;;    word is <quadrand,slot>,, <number of active pages>
  ;;   unfortunately, for the time being, the world still has to agree implicitly
  ;;   on a slot number for the lowest memory, which holds this data!
  ;; locations 700-777 are reserved for disk CCW's
  ;;   locations 700-720 used for swap out.
  ;;   locations 740-760 used for swap in.
  ;;   location 777 is used during booting, etc.
  ;;   (other, higher, locations are used temporarily during band copying.)
  ;;   on CADR, CC-DISK-XFER uses locations 12-377 for the CCW.
  ;;      DCHECK, etc, use 777 for CCW.

  %SYS-COM-AREA-ORIGIN-PNTR		;ADDRESS OF AREA-ORIGIN AREA
  %SYS-COM-VALID-SIZE			;IN A SAVED BAND, NUMBER OF WORDS USED
  					; note in a new format band, this is
					; no longer the highest virtual address.
  %SYS-COM-PAGE-TABLE-PNTR		;ADDRESS OF PAGE-TABLE-AREA
  %SYS-COM-PAGE-TABLE-SIZE		;NUMBER OF QS
  %SYS-COM-OBARRAY-PNTR			;CURRENT OBARRAY, COULD BE AN ARRAY-POINTER
					;BUT NOW IS USUALLY A SYMBOL WHOSE VALUE
					;IS THE CURRENTLY-SELECTED OBARRAY (PACKAGE)
  ;; Ether net interrupt-handler variables
  %SYS-COM-ETHER-FREE-LIST
  %SYS-COM-ETHER-TRANSMIT-LIST
  %SYS-COM-ETHER-RECEIVE-LIST

  %SYS-COM-BAND-FORMAT			;In a saved band, encodes format number.
  					;  1000 -> new compressed format
					;   otherwise old expanded format.
					;In old bands, this is not really initialized
					; but is usually 410.

  %SYS-COM-GC-GENERATION-NUMBER		;reserved for value of %GC-GENERATION-NUMBER

  %SYS-COM-UNIBUS-INTERRUPT-LIST	;SEE LMIO;UNIBUS (LIST OF UNIBUS CHANNELS)

  %SYS-COM-TEMPORARY			;Microcode bashes this at EXTRA-PDL-PURGE

  %SYS-COM-FREE-AREA#-LIST		;Threaded through AREA-REGION-LIST, End=0
  %SYS-COM-FREE-REGION#-LIST		;Threaded through REGION-LIST-THREAD, End=0
  %SYS-COM-MEMORY-SIZE			;Number of words of main memory
  %SYS-COM-WIRED-SIZE			;Number words of low memory wired down
					; Not all of these words are wired; this is
					; really the virtual address of the start
					; of normal pageable memory

  ;; Chaos net interrupt-handler variables
  %SYS-COM-CHAOS-FREE-LIST
  %SYS-COM-CHAOS-TRANSMIT-LIST
  %SYS-COM-CHAOS-RECEIVE-LIST

  ;; Debugger locations  (*** these seem not to be used ***)
  %SYS-COM-DEBUGGER-REQUESTS		;REQUEST TO POWER CONTROL/DEBUGGER
  %SYS-COM-DEBUGGER-KEEP-ALIVE		;KEEP ALIVE FLAG WORD
  %SYS-COM-DEBUGGER-DATA-1		;FOR INTERCOMMUNICATION
  %SYS-COM-DEBUGGER-DATA-2

  %SYS-COM-MAJOR-VERSION		;Major version number of SYSTEM.
					; Was not set up before 98.9 or so.
  %SYS-COM-DESIRED-MICROCODE-VERSION	;Microcode version this world expects
					; Note: this word may be stored with its data type
					; field starting at bit 24 even though pointer
					; fields are now 25 bits!

  ;; To be added:
  ;; Swap out scheduler and disk stuff
  ;; Eventually this may replace SCRATCH-PAD-INIT-AREA
  ;; Those of these that don't need to survive warm boot could be in A-MEMORY
  %SYS-COM-HIGHEST-VIRTUAL-ADDRESS	;In new band format.  You better have this amt of
					; room in the paging partition.
  %SYS-COM-POINTER-WIDTH		;Either 24 or 25, as fixnum, or DTP-FREE in old sys.
  ;; 6 left
  ))

;(AND (> (LENGTH SYSTEM-COMMUNICATION-AREA-QS) 40)
;     (ERROR "System Communication Area Overflow"))

;;; Used by micro assembler.
(cl:defvar MICRO-CODE-SYMBOL-AREA-SIZE 2000)

;;; The value of ARRAY-INDEX-ORDER that a cold load or microassembly is being made for.
(cl:defvar NEW-ARRAY-INDEX-ORDER T)

;;; This list had better be in the same order as the corresponding variables in the UCODE. 
(cl:defvar AREA-LIST '(
  RESIDENT-SYMBOL-AREA				;T and NIL
  SYSTEM-COMMUNICATION-AREA			;Used by paging, console, pdp10 i/o, etc.
  SCRATCH-PAD-INIT-AREA				;Load micro code variables upon startup
  MICRO-CODE-SYMBOL-AREA			;600 Qs misc dispatch, ucode entry dispatch
  ;; MICRO-CODE-SYMBOL-AREA is considered part of the microcode, not the band.
  ;; the disk-restore microcode knows about it, and its length.
  REGION-ORIGIN					;Fixnum base address indexed by region #
  REGION-LENGTH					;Fixnum length indexed by region #
  REGION-BITS					;Fixnum, see %%REGION- syms for fields
  REGION-FREE-POINTER				;Fixnum, relative allocation point.
  ;; Below here must not be clobbered by DISK-COPY routines in the ucode.
  PAGE-TABLE-AREA				;Page hash table
  PHYSICAL-PAGE-DATA				;GC-DATA,,PHT-INDEX
						; -1 if out of service
						; PHT-INDEX=-1 if fixed-wired (no PHT entry)
						; GC-DATA=0 if not in use
  ADDRESS-SPACE-MAP				;See %ADDRESS-SPACE-MAP-BYTE-SIZE below
  ;; End wired areas
  REGION-GC-POINTER				;Gc use, mainly relative dirty/clean boundary
  REGION-LIST-THREAD				;Next region# in area, or 1_23.+area#
						; Threads free region slots, too.
  AREA-NAME					;Atomic name indexed by area #
  AREA-REGION-LIST				;First region# in area
  AREA-REGION-BITS				;Get region-bits of new regions from this.
  AREA-REGION-SIZE				;Recommended size for new regions
  AREA-MAXIMUM-SIZE				;Approximate maximum #wds allowed in this area
  SUPPORT-ENTRY-VECTOR				;Constants needed by basic microcode
  CONSTANTS-AREA				;Common constants used by macrocode
  ;; NOTE!! EXTRA-PDL-AREA must end on a address space quantuum boundary!!
  EXTRA-PDL-AREA				;Separately gc-able area, mainly extended nums
						; Must be right before MICRO-CODE-ENTRY-AREA
  MICRO-CODE-ENTRY-AREA				;Micro entry address
						; Or locative indirect MICRO-CODE-SYMBOL-AREA
  MICRO-CODE-ENTRY-NAME-AREA			;Micro entry name
  MICRO-CODE-ENTRY-ARGS-INFO-AREA		;Micro entry %ARGS-INFO
  MICRO-CODE-ENTRY-MAX-PDL-USAGE		;Micro entry pdl depth incl micro-micro calls
  MICRO-CODE-PAGING-AREA			;Hold virtual microcode memory.
  PAGE-GC-BITS					;Bits recording what ptrs exist in each page
  PAGE-STRUCTURE-HANDLES			;Location of first structure on each page
  ;; Areas after here are not "initial"; not known specially by microcode 
  MICRO-CODE-ENTRY-ARGLIST-AREA			;Value for arglist function to return
  MICRO-CODE-SYMBOL-NAME-AREA			;Names of micro-code-symbol-area entries
  LINEAR-PDL-AREA				;Main pdl
  LINEAR-BIND-PDL-AREA				;Corresponding bind pdl
  INIT-LIST-AREA				;List constants created by cold load
  ;; End fixed areas, which must have only one region
  WORKING-STORAGE-AREA				;Ordinary consing happens here
  PERMANENT-STORAGE-AREA			;Put "permanent" data structures here
  PROPERTY-LIST-AREA				;Exists for paging Reasons
  P-N-STRING					;Print names and strings
  CONTROL-TABLES				;Obarray, readtable (semi-obsolete)
  NR-SYM					;Symbols not in resident-symbol-area
  MACRO-COMPILED-PROGRAM			;Macro code loaded here
  PDL-AREA					;Put stack-group regular-pdls here
  FASL-TABLE-AREA				;Fasload's table is here
  FASL-TEMP-AREA				;Fasload temporary consing
  ))

;;; Default area size is one page
(cl:defvar COLD-LOAD-AREA-SIZES '(
  P-N-STRING			600
  NR-SYM			500
  MACRO-COMPILED-PROGRAM	1000
  PAGE-TABLE-AREA		128.		;Enough for 2 megawords of main memory
  PHYSICAL-PAGE-DATA		32.		;Enough for 2 megawords of main memory
  ADDRESS-SPACE-MAP		2		;Must start on a level-2 map boundary.
  LINEAR-PDL-AREA		100
  LINEAR-BIND-PDL-AREA		10
  PDL-AREA 			300
  WORKING-STORAGE-AREA		400
  PERMANENT-STORAGE-AREA	200
  PROPERTY-LIST-AREA		100
  CONTROL-TABLES		13
  INIT-LIST-AREA 		340
  MICRO-CODE-ENTRY-AREA		4
  MICRO-CODE-ENTRY-NAME-AREA	4
  MICRO-CODE-ENTRY-ARGS-INFO-AREA	4
  MICRO-CODE-ENTRY-ARGLIST-AREA	4
  MICRO-CODE-ENTRY-MAX-PDL-USAGE	4
  MICRO-CODE-SYMBOL-NAME-AREA	4
  MICRO-CODE-SYMBOL-AREA	5
  MICRO-CODE-PAGING-AREA	1000
  PAGE-GC-BITS			40
  PAGE-STRUCTURE-HANDLES        200
  FASL-TABLE-AREA		201		;3 times length-of-fasl-table plus 1 page
  EXTRA-PDL-AREA		111  ;NOTE!! this is carefully calculated to cause
				   ; EXTRA-PDL-AREA to end on a level-2 map boundary (200000)
  FASL-TEMP-AREA 		40
  ))

;;; Next three symbols are treated bletcherously, because there isnt the right kind of
;;; LDB available

;;; VIRTUAL ADDRESS OF 0@A.  MUST AGREE WITH VALUE IN UCADR.
;;; (unfortunately called LOWEST-A-MEM-VIRTUAL-ADDRESS).
;;; Virtual address of X-BUS IO space.
;;; Must agree with LOWEST-IO-SPACE-VIRTUAL-ADDRESS in UCADR.
;;; Virtual address of UNIBUS IO space.
;;; Must agree with LOWEST-UNIBUS-VIRTUAL-ADDRESS in UCADR.

; old 24-bit pointer values
;(GLOBAL:IF (GLOBAL:NOT GLOBAL:(OR (EQ PACKAGE (FIND-PACKAGE "SYM"))
;		      (> %MICROCODE-VERSION-NUMBER 309.))
;(DEFCONST A-MEMORY-VIRTUAL-ADDRESS 76776000)
;(DEFCONST IO-SPACE-VIRTUAL-ADDRESS 77000000)
;(DEFCONST UNIBUS-VIRTUAL-ADDRESS 77400000)

(cl:defvar A-MEMORY-VIRTUAL-ADDRESS 176776000) ; (%P-LDB-OFFSET 0031 176776000 1))
(cl:defvar IO-SPACE-VIRTUAL-ADDRESS 177000000) ; (%P-LDB-OFFSET 0031 177000000 1))
(cl:defvar UNIBUS-VIRTUAL-ADDRESS 177400000) ; (%P-LDB-OFFSET 0031 177400000 1))
						; doing an (ENABLE-TRAPPING)

(cl:defvar MULTIBUS-VIRTUAL-ADDRESS 177400000) ; (%P-LDB-OFFSET 0031 177400000 1))

(cl:defvar HEADER-FIELD-VALUES '(%%HEADER-TYPE-FIELD 2305 %%HEADER-REST-FIELD 0023))
(cl:defvar HEADER-FIELDS (GET-ALTERNATE HEADER-FIELD-VALUES))

;;; These are the values that go in the %%HEADER-TYPE-FIELD of a Q of
;;; data type DTP-HEADER.
(cl:defvar Q-HEADER-TYPES '(
  %HEADER-TYPE-ERROR
  %HEADER-TYPE-FEF
  %HEADER-TYPE-ARRAY-LEADER
  %HEADER-TYPE-unused
  %HEADER-TYPE-FLONUM
  %HEADER-TYPE-COMPLEX
  %HEADER-TYPE-BIGNUM
  %HEADER-TYPE-RATIONAL
  %HEADER-TYPE-FAST-FEF-FIXED-ARGS-NO-LOCALS
  %HEADER-TYPE-FAST-FEF-VAR-ARGS-NO-LOCALS
  %HEADER-TYPE-FAST-FEF-FIXED-ARGS-WITH-LOCALS
  %HEADER-TYPE-FAST-FEF-VAR-ARGS-WITH-LOCALS
  ))

;;; These three lists describing the possible types of "argument descriptor info"
(cl:defvar ADI-KINDS '(
  ADI-ERR
  ADI-RETURN-INFO
  ADI-RESTART-PC
  ADI-FEXPR-CALL 
  ADI-LEXPR-CALL
  ADI-BIND-STACK-LEVEL
  ADI-UNUSED-6
  ADI-USED-UP-RETURN-INFO
  ))

(cl:defvar ADI-STORING-OPTIONS '(
  ADI-ST-ERR
  ADI-ST-BLOCK
  ADI-ST-LIST 
  ADI-ST-MAKE-LIST
  ADI-ST-INDIRECT
  ))

(cl:defvar ADI-FIELD-VALUES '(
  %%ADI-TYPE			2403
  %%ADI-RET-STORING-OPTION	2103 
  %%ADI-PREVIOUS-ADI-FLAG	3601		;Overlaps cdr-code which isn"t used in ADI words.
  %%ADI-RET-SWAP-SV		2001
  %%ADI-RET-NUM-VALS-TOTAL 	0606		;For ADI-ST-BLOCK; total number of values wanted.
  %%ADI-RET-NUM-VALS-EXPECTING	0006		;For ADI-ST-BLOCK; number of values still room for.
  %%ADI-RPC-MICRO-STACK-LEVEL	0006
  ))
(ASSIGN-ALTERNATE ADI-FIELD-VALUES)
(cl:defvar ADI-FIELDS (GET-ALTERNATE ADI-FIELD-VALUES))

;;; These overlap the cdr-code field, which is not used in the special pdl.
(cl:defvar SPECPDL-FIELD-VALUES '(
  %%SPECPDL-BLOCK-START-FLAG 3601		;Flag is set on first binding of each block of bindings
  %%SPECPDL-CLOSURE-BINDING 3701		;Flag is set on bindings made "before" entering function
  ))
(ASSIGN-ALTERNATE SPECPDL-FIELD-VALUES)
(cl:defvar SPECPDL-FIELDS (GET-ALTERNATE SPECPDL-FIELD-VALUES))

;;; LINEAR-PDL-QS and LINEAR-PDL-FIELDS, and their elements, go in the real machine.
(cl:defvar LINEAR-PDL-QS '(
  %LP-FEF
  %LP-ENTRY-STATE
  %LP-EXIT-STATE
  %LP-CALL-STATE
  ))
;;; These are assigned values starting with 0 and incremented by -1
(ASSIGN-VALUES-INIT-DELTA LINEAR-PDL-QS 0 0 -1)

(cl:defvar %LP-CALL-BLOCK-LENGTH (cl:LENGTH LINEAR-PDL-QS))
(cl:defvar LLPFRM 4)	;Number of fixed words in a linear call block. (Obsolete, use above)

(cl:defvar %LP-INITIAL-LOCAL-BLOCK-OFFSET 1)

(cl:defvar LINEAR-PDL-FIELDS-VALUES '(
  ;LPCLS (%LP-CALL-STATE).  Stored when this call frame is created.
  ;; Set if any of the following bits are set (used for fast check when returning from call):
  ;;   TRAP-ON-EXIT, ADI-PRESENT, MICRO-STACK-SAVED, BINDING-BLOCK-PUSHED,
  ;;   ENVIRONMENT-POINTER-POINTS-HERE, or function exit/entry metering is enabled,
  ;;   or this frame just needs to be unwound.
  %%LP-CLS-ATTENTION 3001
  ;; If set, need not compute SELF-MAPPING-TABLE
  ;;  because our caller has done so.
  %%LP-CLS-SELF-MAP-PROVIDED 2701
  ;; If set, get error before popping this frame.
  %%LP-CLS-TRAP-ON-EXIT 2601
  ;; ADI words precede this call-block
  %%LP-CLS-ADI-PRESENT 2401
  ;; Where in the caller to put this frame's value
  %%LP-CLS-DESTINATION 2004
  ;; This includes the destination field and ADI bit.
  %%LP-CLS-DESTINATION-AND-ADI 2005
  ;; Offset back to previous open or active block
  ;; An open block is one whose args are being made
  %%LP-CLS-DELTA-TO-OPEN-BLOCK 1010
  ;; Offset back to previous active block
  ;;  An active block is one that is executing
  %%LP-CLS-DELTA-TO-ACTIVE-BLOCK 0010
  ;LPEXS (%LP-EXIT-STATE).  Stored when this frame calls out.
  ; bits 22'-27' not used in LPEXS
  ;; A microstack frame exists on special pdl
  %%LP-EXS-MICRO-STACK-SAVED 2101
  ;; Same as below
  %%LP-EXS-PC-STATUS 2001
  ;; M-QBBFL STORED HERE IN MACRO EXIT OPERATION 
  %%LP-EXS-BINDING-BLOCK-PUSHED 2001
  ;; LC as offset in halfwords from FEF
  ;;  Meaningless if %LP-FEF not a fef.
  ;;  Don't change %%LP-EXS-EXIT-PC ---  the numerical value is known by UCADR
  %%LP-EXS-EXIT-PC 0017
  ;LPENS (%LP-ENTRY-STATE).  Stored when this frame entered.
  ; bits 21'-27' not used in LPENS
  ;; This is nonzero if an explicit rest arg is passed.
  %%LP-ENS-LCTYP 2001
  ;; Here are the fields that the entry state normally contains.
  ;; This is 1 if this frame has a rest arg living on the stack.
  ;; Means this frame cannot be flushed for tail recursion.
  %%LP-ENS-UNSAFE-REST-ARG 1701
  ;; This includes the number-of-args field and the unsafe field.
  %%LP-ENS-NUM-ARGS-AND-UNSAFE-FLAG 1010
  ;; This is a pointer to the unsafe flag, within the byte that goes
  ;; into the %%lp-ens-num-args-and-unsafe-flag field.
  %%LP-ENS-UNSAFE-REST-ARG-1 0701
  %%LP-ENS-ENVIRONMENT-POINTER-POINTS-HERE 1601
  %%LP-ENS-NUM-ARGS-SUPPLIED 1006
  %%LP-ENS-MACRO-LOCAL-BLOCK-ORIGIN 0010
  ))

(ASSIGN-ALTERNATE LINEAR-PDL-FIELDS-VALUES)
(cl:defvar LINEAR-PDL-FIELDS (GET-ALTERNATE LINEAR-PDL-FIELDS-VALUES))

;;; MICRO-STACK-FIELDS and its elements go in the real machine.
(cl:defvar MICRO-STACK-FIELDS-VALUES '(
  %%US-RPC 1600					;Return PC
  %%US-MACRO-INSTRUCTION-RETURN 1601		;Triggers instruction-stream stuff
  %%US-PPBMIA 1701				;ADI on micro-to-micro-call
  %%US-PPBSPC 2101				;Binding block pushed
  ))

(ASSIGN-ALTERNATE MICRO-STACK-FIELDS-VALUES)
(cl:defvar MICRO-STACK-FIELDS (GET-ALTERNATE MICRO-STACK-FIELDS-VALUES))


;;;; M-FLAGS-FIELDS and M-ERROR-SUBSTATUS-FIELDS and their elements go in the real machine.
(cl:defvar M-FLAGS-FIELDS-VALUES '(		;MUST AGREE WITH DEFS IN UCONS
  %%M-FLAGS-QBBFL 0001				;Bind block open flag
  %%M-FLAGS-CAR-SYM-MODE 0102			;CAR of symbol gives: error, error except 
						; (CAR NIL) -> nil, nil, p-name pointer
  %%M-FLAGS-CAR-NUM-MODE 0302			;CAR of number gives: error, nil, "what it is"
  %%M-FLAGS-CDR-SYM-MODE 0502			;CDR of symbol gives: error, error except
						; (cdr NIL) -> NIL, NIL, PROPERTY-LIST
  %%M-FLAGS-CDR-NUM-MODE 0702			;CDR of number gives: error, nil, "what it is"
  %%M-FLAGS-DONT-SWAP-IN 1101			;MAGIC FLAG FOR CREATING FRESH PAGES
  %%M-FLAGS-TRAP-ENABLE 1201			;1 enable error trapping
  %%M-FLAGS-MAR-MODE 1302			;1-BIT = read-trap, 2-BIT = write-trap
  %%M-FLAGS-PGF-WRITE 1501			;Flag used by page fault routine
  %%M-FLAGS-INTERRUPT 1601			;In microcode interrupt
  %%M-FLAGS-SCAVENGE 1701			;In scavenger
  %%M-FLAGS-TRANSPORT 2001			;In transporter
  %%M-FLAGS-STACK-GROUP-SWITCH 2101		;Switching stack groups
  %%M-FLAGS-DEFERRED-SEQUENCE-BREAK 2201	;Sequence break pending but inhibited
  %%M-FLAGS-METER-ENABLE 2301			;Metering enabled for this stack group
  %%M-FLAGS-TRAP-ON-CALL 2401			;Trap on attempting to activate new frame.
  ))
(ASSIGN-ALTERNATE M-FLAGS-FIELDS-VALUES)
(cl:defvar M-FLAGS-FIELDS (GET-ALTERNATE M-FLAGS-FIELDS-VALUES))

(cl:defvar M-ERROR-SUBSTATUS-FIELDS-VALUES '(	;MUST AGREE WITH DEFS IN UCONS
  %%M-ESUBS-TOO-FEW-ARGS 0001
  %%M-ESUBS-TOO-MANY-ARGS 0101
  %%M-ESUBS-BAD-QUOTED-ARG 0201 
  %%M-ESUBS-BAD-EVALED-ARG 0301
  %%M-ESUBS-BAD-DT 0401 
  %%M-ESUBS-BAD-QUOTE-STATUS 0501
  ))
(ASSIGN-ALTERNATE M-ERROR-SUBSTATUS-FIELDS-VALUES)
(cl:defvar M-ERROR-SUBSTATUS-FIELDS (GET-ALTERNATE M-ERROR-SUBSTATUS-FIELDS-VALUES))

;;; A "Numeric Argument Description" is what %ARGS-INFO and ARGS-INFO return.
;;; Such descriptors can also be hung on symbols' Q-ARGS-PROP properties.
;;; The "fast option Q" of a FEF is stored in this format.
;;; These symbols go in the real machine.
(cl:defvar NUMERIC-ARG-DESC-INFO '(
  %ARG-DESC-QUOTED-REST 10000000		;HAS QUOTED REST ARGUMENT
  %%ARG-DESC-QUOTED-REST 2501
  %ARG-DESC-EVALED-REST 4000000			;HAS EVALUATED REST ARGUMENT
  %%ARG-DESC-EVALED-REST 2401
  %%ARG-DESC-ANY-REST 2402			;NON-ZERO IF HAS EITHER KIND OF REST ARG
  %ARG-DESC-FEF-QUOTE-HAIR 2000000		;MACRO COMPILED FCN WITH HAIRY QUOTING,
  %%ARG-DESC-FEF-QUOTE-HAIR 2301		; CALLER MUST CHECK A-D-L FOR FULL INFO
  %ARG-DESC-INTERPRETED 1000000			;THIS IS INTERPRETED FUNCTION, 
  %%ARG-DESC-INTERPRETED 2201			; NO INFORMATION AVAILABLE (VAL=1000077)
  %ARG-DESC-FEF-BIND-HAIR 400000		;MACRO COMPILED FCN WITH HAIRY BINDING,
  %%ARG-DESC-FEF-BIND-HAIR 2101			; LINEAR ENTER MUST CHECK A-D-L
  %%ARG-DESC-MIN-ARGS 0606			;MINIMUM NUMBER OF REQUIRED ARGS
  %%ARG-DESC-MAX-ARGS 0006			;MAXIMUM NUMBER OF REQUIRED+OPTIONAL
						; ARGS.  REST ARGS NOT COUNTED.
  ))
(ASSIGN-ALTERNATE NUMERIC-ARG-DESC-INFO)
(cl:defvar NUMERIC-ARG-DESC-FIELDS (GET-ALTERNATE NUMERIC-ARG-DESC-INFO))

(cl:defvar ARG-DESC-FIELD-VALUES '(
  %FEF-ARG-SYNTAX 160
  %FEF-QUOTE-STATUS 600 
  %FEF-DES-DT 17000 
  %FEF-INIT-OPTION 17
  %FEF-SPECIAL-BIT #.(cl:ash 1 16)
  %FEF-NAME-PRESENT #.(cl:ash 1 20)
  ;; ***UNFORTUNATELY, ASSIGN-COMP-VALUES KNOWS ABOUT THESE TOO****
  %%FEF-NAME-PRESENT 2001
  %%FEF-SPECIAL-BIT 1601
  %%FEF-SPECIALNESS 1602
  %%FEF-FUNCTIONAL 1501
  %%FEF-DES-DT 1104 
  %%FEF-QUOTE-STATUS 0702
  %%FEF-ARG-SYNTAX 0403
  %%FEF-INIT-OPTION 0004 
  ))
(ASSIGN-ALTERNATE ARG-DESC-FIELD-VALUES)
(cl:defvar ARG-DESC-FIELDS (GET-ALTERNATE ARG-DESC-FIELD-VALUES))
	;ARG-DESC-FIELDS GETS SET TO A LIST CONSISTING OF THE ALTERNATING MEMBERS OF 
	;ARG-DESC-FIELD-VALUES

(cl:defvar FEF-NAME-PRESENT '(
  FEF-NM-NO
  FEF-NM-YES
  ))
(cl:defvar FEF-SPECIALNESS '(
  FEF-LOCAL
  FEF-SPECIAL
  FEF-SPECIALNESS-UNUSED
  FEF-REMOTE
  ))
(cl:defvar FEF-FUNCTIONAL '(
  FEF-FUNCTIONAL-DONTKNOW
  FEF-FUNCTIONAL-ARG
  ))
(cl:defvar FEF-DES-DT '(
  FEF-DT-DONTCARE
  FEF-DT-NUMBER
  FEF-DT-FIXNUM
  FEF-DT-SYM
  FEF-DT-ATOM
  FEF-DT-LIST
  FEF-DT-FRAME
  ))
(cl:defvar FEF-QUOTE-STATUS '(
  FEF-QT-DONTCARE
  FEF-QT-EVAL
  FEF-QT-QT
  ))
(cl:defvar FEF-ARG-SYNTAX '(
  FEF-ARG-REQ
  FEF-ARG-OPT
  FEF-ARG-REST
  FEF-ARG-AUX
  FEF-ARG-FREE 
  FEF-ARG-INTERNAL
  FEF-ARG-INTERNAL-AUX
  ))
(cl:defvar FEF-INIT-OPTION '(
  FEF-INI-NONE
  FEF-INI-NIL
  FEF-INI-PNTR
  FEF-INI-C-PNTR
  FEF-INI-OPT-SA
  FEF-INI-COMP-C
  FEF-INI-EFF-ADR
  FEF-INI-SELF
  ))


(cl:defvar ARRAY-FIELD-VALUES '(
  %%ARRAY-TYPE-FIELD 2305
  %%ARRAY-LEADER-BIT 2101 
  %%ARRAY-DISPLACED-BIT 2001
  %%ARRAY-FLAG-BIT 1701 
  %%ARRAY-NUMBER-DIMENSIONS 1403
  %%ARRAY-LONG-LENGTH-FLAG 1301
  %%ARRAY-NAMED-STRUCTURE-FLAG 1201
  %%ARRAY-INDEX-LENGTH-IF-SHORT 0012
  %ARRAY-MAX-SHORT-INDEX-LENGTH 1777
  ))

(cl:defvar ARRAY-LEADER-FIELD-VALUES '(
  %ARRAY-LEADER-LENGTH 777777 
  %%ARRAY-LEADER-LENGTH 0022
  %%ARRAY-LEADER-FUNCALL-AS-HASH-TABLE 2201
  ))

(cl:defvar ARRAY-MISC-VALUES '(
  ARRAY-DIM-MULT #.(cl:ash 1 14)
  ARRAY-DIMENSION-SHIFT -14 
  ARRAY-TYPE-SHIFT -23
  ARRAY-LEADER-BIT #.(cl:ash 1 21)
  ARRAY-DISPLACED-BIT #.(cl:ash 1 20)
  ARRAY-LONG-LENGTH-FLAG #.(cl:ash 1 13)
  ARRAY-NAMED-STRUCTURE-FLAG #.(cl:ash 1 12)))

(cl:defvar ARRAY-FIELDS (GET-ALTERNATE ARRAY-FIELD-VALUES))
(cl:defvar ARRAY-LEADER-FIELDS (GET-ALTERNATE ARRAY-LEADER-FIELD-VALUES))
(cl:defvar ARRAY-MISCS (GET-ALTERNATE ARRAY-MISC-VALUES))

(cl:defvar ARRAY-TYPES '(
  ART-ERROR
  ART-1B
  ART-2B
  ART-4B
  ART-8B
  ART-16B
  ART-32B 
  ART-Q
  ART-Q-LIST
  ART-STRING
  ART-STACK-GROUP-HEAD
  ART-SPECIAL-PDL
  ART-HALF-FIX
  ART-REG-PDL
  ART-FLOAT
  ART-FPS-FLOAT
  ART-FAT-STRING
  ART-COMPLEX-FLOAT
  ART-COMPLEX
  ART-COMPLEX-FPS-FLOAT
  ))

(cl:defvar ARRAY-ELEMENTS-PER-Q '(
  (ART-Q . 1)
  (ART-STRING . 4)
  (ART-1B . 40)
  (ART-2B . 20)
  (ART-4B . 10)
  (ART-8B . 4)
  (ART-16B . 2)
  (ART-32B . 1)
  (ART-Q-LIST . 1) 
  (ART-STACK-GROUP-HEAD . 1)
  (ART-SPECIAL-PDL . 1)
  (ART-HALF-FIX . 2) 
  (ART-REG-PDL . 1)
  (ART-FLOAT . -2)
  (ART-FPS-FLOAT . 1)
  (ART-FAT-STRING . 2)
  (ART-COMPLEX-FLOAT . -4)
  (ART-COMPLEX . -2)
  (ART-COMPLEX-FPS-FLOAT . -2)
  ))

;;; NIL for Q-type arrays
(cl:defvar ARRAY-BITS-PER-ELEMENT '(
  (ART-Q . NIL)
  (ART-STRING . 8)
  (ART-1B . 1)
  (ART-2B . 2)
  (ART-4B . 4)
  (ART-8B . 8)
  (ART-16B . 16.)
  (ART-32B . 24.)
  (ART-Q-LIST . NIL) 
  (ART-STACK-GROUP-HEAD . NIL)
  (ART-SPECIAL-PDL . NIL)
  (ART-HALF-FIX . 16.)
  (ART-REG-PDL . NIL)
  (ART-FLOAT . 32.)
  (ART-FPS-FLOAT . 32.)
  (ART-FAT-STRING . 16.)
  (ART-COMPLEX-FLOAT . 32.)
  (ART-COMPLEX . 32.)
  (ART-COMPLEX-FPS-FLOAT . 32.)))

;;; Fields in a DTP-SELF-REF-POINTER.  RELOCATE-FLAG says use SELF-MAPPING-TABLE;
;;; INDEX is slot number in self, or index in mapping table.
;;; the WORD-INDEX is the index divided by two, to index by words in mapping table.
;;; The map-leader-flag says to get the contents of an array leader slot
;;; of the mapping table; in this case, the index is the leader slot number.

;;; If %%SELF-REF-MONITOR-FLAG is set, this is a monitor pointer.
;;; It acts like a one-q forward to the following word,
;;; except that all write references get a continuable error.
(cl:defvar SELF-REF-POINTER-FIELD-VALUES '(
  %%SELF-REF-RELOCATE-FLAG 2301
  %%SELF-REF-MAP-LEADER-FLAG 2201
  %%SELF-REF-MONITOR-FLAG 2101
  %%SELF-REF-INDEX 0014
  %%SELF-REF-WORD-INDEX 0113
  ))
(ASSIGN-ALTERNATE SELF-REF-POINTER-FIELD-VALUES)
(cl:defvar SELF-REF-POINTER-FIELDS (GET-ALTERNATE SELF-REF-POINTER-FIELD-VALUES))

;;; FEF header fields
(cl:defvar FEFH-CONSTANT-VALUES '(
  %FEFH-PC 77777				;There are 19 available bits in this word!
  %FEFH-NO-ADL #.(cl:ash 1 18.)
  %FEFH-FAST-ARG #.(cl:ash 1 17.)
  %FEFH-SV-BIND #.(cl:ash 1 16.)
  %%FEFH-PC 0017
  %%FEFH-PC-IN-WORDS 0116
  %%FEFH-NO-ADL 2201
  %%FEFH-GET-SELF-MAPPING-TABLE 1701		;Mapping table flavor name precedes ADL.
  %%FEFH-FAST-ARG 2101
  %%FEFH-SV-BIND 2001
  ))
(ASSIGN-ALTERNATE FEFH-CONSTANT-VALUES)
(cl:defvar FEFH-CONSTANTS (GET-ALTERNATE FEFH-CONSTANT-VALUES))

;;; Fast FEF header fields.
(cl:defvar FAST-FEFH-CONSTANT-VALUES '(
; Bits used for info are 3602 (cdr-code), 1704.  3101 is wasted because header-type is
; in the old (24-bit style) position.  The PC fields from the slow case apply here, but
; the GET-SELF-MAPPING-TABLE, SV-BIND, FAST-ARG, NO-ADL, bits do not.
  %%FEFH-ARGS-FOR-FANL 1704		;Number of args for FIXED-ARGS-NO-LOCALS.
  %%FEFH-MIN-ARGS-FOR-VANL 3602		;Minimum number of args for VAR-ARGS-NO-LOCALS.
  %%FEFH-MAX-ARGS-FOR-VANL 1704		;Maximum number of args for VAR-ARGS-NO-LOCALS.
  %%FEFH-ARGS-FOR-FAWL 3602		;Number of args for FIXED-ARGS-WITH-LOCALS.
  %%FEFH-LOCALS-FOR-FAWL 1704		;Local block length for FIXED-ARGS-WITH-LOCALS.
  %%FEFH-MIN-ARGS-FOR-VAWL 3602		;Minimum number of args for VAR-ARGS-WITH-LOCALS.
  %%FEFH-MAX-ARGS-FOR-VAWL 1702		;Maximum number of args for VAR-ARGS-WITH-LOCALS.
  %%FEFH-LOCALS-FOR-VAWL 2102		;Local block length for VAR-ARGS-WITH-LOCALS.
  %%FEFSL-NO-ADL 3701			;New NO-ADL field.
  ))
(ASSIGN-ALTERNATE FAST-FEFH-CONSTANT-VALUES)
(cl:defvar FAST-FEFH-CONSTANTS (GET-ALTERNATE FAST-FEFH-CONSTANT-VALUES))

;;; FEF header q indexes
(cl:defvar FEFHI-INDEXES '(
  %FEFHI-IPC
  %FEFHI-STORAGE-LENGTH
  %FEFHI-FCTN-NAME
  %FEFHI-FAST-ARG-OPT
  %FEFHI-SV-BITMAP
  %FEFHI-MISC
  %FEFHI-SPECIAL-VALUE-CELL-PNTRS
  ))

(cl:defvar IFEFOFF (cl:1- (cl:length FEFHI-INDEXES)))	;Q'S IN FIXED ALLOC PART OF FEF
(cl:defvar %FEF-HEADER-LENGTH IFEFOFF)		;BETTER NAME FOR ABOVE

(cl:defvar FEFHI-VALUES '(
  %%FEFHI-FSO-MIN-ARGS 0606
  %%FEFHI-FSO-MAX-ARGS 0006 
  %%FEFHI-MS-LOCAL-BLOCK-LENGTH 0007
  %%FEFHI-MS-ARG-DESC-ORG 0710 
  %%FEFHI-MS-BIND-DESC-LENGTH 1710 
  %%FEFHI-MS-DEBUG-INFO-PRESENT 2701
  %%FEFHI-SVM-ACTIVE 2601
  %FEFHI-SVM-ACTIVE #.(cl:ash 1 26)
  %%FEFHI-SVM-BITS 0026
  %%FEFHI-SVM-HIGH-BIT 2501
  ))
(cl:defvar FEFHI-FIELDS (GET-ALTERNATE FEFHI-VALUES))


(cl:defvar PAGE-SIZE 400)

(cl:defvar SIZE-OF-AREA-ARRAYS 377)

;;; Assuming no more than 256 regions
(cl:defvar %ADDRESS-SPACE-MAP-BYTE-SIZE 8.)
(cl:defvar %ADDRESS-SPACE-QUANTUM-SIZE #o40000)
;;; Each quantum has a byte in the ADDRESS-SPACE-MAP area,
;;; which is the region number, or 0 if free or fixed area.
;;; INIT-LIST-AREA is the last fixed area.

;;;; Page table stuff etc.

;;; Definitions of fields in page hash table
(cl:defvar PAGE-VALUES '(
  ;; WORD 1 
  %%PHT1-VIRTUAL-PAGE-NUMBER 1021		;ALIGNED SAME AS VMA
  %PHT-DUMMY-VIRTUAL-ADDRESS 377777		;ALL ONES MEANS THIS IS DUMMY ENTRY
						;WHICH JUST REMEMBERS A FREE CORE PAGE
  %%PHT1-SWAP-STATUS-CODE 0003
  %PHT-SWAP-STATUS-NORMAL 1			;ORDINARY PAGE
  %PHT-SWAP-STATUS-FLUSHABLE 2			;SAFELY REUSABLE TO SWAP PAGES INTO
						;MAY NEED TO BE WRITTEN TO DISK FIRST
  %PHT-SWAP-STATUS-PREPAGE 3			;SAME AS FLUSHABLE, BUT CAME IN VIA PREPAGE
  %PHT-SWAP-STATUS-AGE-TRAP 4			;LIKE NORMAL BUT TRYING TO MAKE FLUSHABLE
  %PHT-SWAP-STATUS-WIRED 5			;NOT SWAPPABLE
  %%PHT1-AGE 0302				;NUMBER OF TIMES AGED
  %%PHT1-MODIFIED-BIT 0501			;1 IF PAGE MODIFIED, BUT THE FACT NOT RECORDED
						; IN THE MAP-STATUS, BECAUSE IT IS NOMINALLY
						;  READ-ONLY OR NOMINALLY READ-WRITE-FIRST.
  %%PHT1-VALID-BIT 0601				;1 IF THIS HASH TABLE SLOT IS OCCUPIED.
  %%PHT1-SCAVENGER-WS-FLAG 0701			;IF SET, PAGE IN SCAVENGER WORKING SET.

  ;; Pht word 2.  This is identical to the level-2 map

  %%PHT2-META-BITS 1606				;SEE %%REGION-MAP-BITS
  %%PHT2-MAP-STATUS-CODE 2403
  %PHT-MAP-STATUS-MAP-NOT-VALID 0		;LEVEL 1 OR 2 MAP NOT SET UP
  %PHT-MAP-STATUS-META-BITS-ONLY 1		;HAS META BITS BUT NO PHYSICAL ADDRESS
  %PHT-MAP-STATUS-READ-ONLY 2			;GARBAGE COLLECTOR CAN STILL WRITE IN IT
  %PHT-MAP-STATUS-READ-WRITE-FIRST 3		;READ/WRITE BUT NOT MODIFIED
  %PHT-MAP-STATUS-READ-WRITE 4			;READ/WRITE AND MODIFIED
  %PHT-MAP-STATUS-PDL-BUFFER 5			;MAY RESIDE IN PDL BUFFER
  %PHT-MAP-STATUS-MAR 6				;MAR SET SOMEWHERE ON THIS PAGE
  %%PHT2-MAP-ACCESS-CODE 2602
  %%PHT2-ACCESS-STATUS-AND-META-BITS 1612
  %%PHT2-ACCESS-AND-STATUS-BITS 2404 
  %%PHT2-PHYSICAL-PAGE-NUMBER 0016
  ))
(ASSIGN-ALTERNATE PAGE-VALUES)
(cl:defvar PAGE-HASH-TABLE-FIELDS (GET-ALTERNATE PAGE-VALUES))

;;; See SYS2;SGDEFS
(cl:defvar STACK-GROUP-HEAD-LEADER-QS '(
  SG-NAME
  SG-REGULAR-PDL
  SG-REGULAR-PDL-LIMIT
  SG-SPECIAL-PDL
  SG-SPECIAL-PDL-LIMIT
  SG-INITIAL-FUNCTION-INDEX
  SG-PLIST
  ;; End static section, begin debugging section
  SG-TRAP-TAG					;Symbolic tag corresponding to
						; SG-TRAP-MICRO-PC. Gotten via
						; MICROCODE-ERROR-TABLE, etc.  Properties of
						; this symbol drive various stages in error
						; recovery, etc.
  SG-RECOVERY-HISTORY				;Available for hairy SG munging routines to
						; leave tracks in for debugging purposes.
  SG-FOOTHOLD-DATA				;Structure which saves dynamic section of
						;  "real" SG when executing in the foothold. 
  ;; Locations below here are actually loaded/stored on sg-enter/sg-leave
  ;; End debugging section, begin "high level" section
  SG-STATE
  SG-PREVIOUS-STACK-GROUP
  SG-CALLING-ARGS-POINTER 
  SG-CALLING-ARGS-NUMBER
  ;SG-FOLLOWING-STACK-GROUP 
  SG-TRAP-AP-LEVEL
  ;; End high-level section, begin "dynamic" section
  ;; --Below here is saved in SG-FOOTHOLD-DATA when
  ;; %%SG-ST-FOOTHOLD-EXECUTING is set.
  SG-REGULAR-PDL-POINTER
  SG-SPECIAL-PDL-POINTER
  SG-AP SG-IPMARK 
  SG-TRAP-MICRO-PC				;PC saved from OPCS at micro-location TRAP
  ;SG-ERROR-HANDLING-SG
  ;SG-INTERRUPT-HANDLING-SG 
  ;       HAVING THESE BE PART OF THE SG IS BASICALLY A GOOD IDEA, BUT IT
  ;       DOESNT BUY ANYTHING FOR THE TIME BEING AND COSTS A COUPLE OF MICROINSTRUCTIONS
  SG-SAVED-QLARYH
  SG-SAVED-QLARYL
  SG-SAVED-M-FLAGS
  SG-AC-K
  SG-AC-S
  SG-AC-J 
  SG-AC-I
  SG-AC-Q
  SG-AC-R
  SG-AC-T
  SG-AC-E
  SG-AC-D
  SG-AC-C 
  SG-AC-B
  SG-AC-A
  SG-AC-ZR
  SG-AC-2
  SG-AC-1
  SG-VMA-M1-M2-TAGS
  SG-SAVED-VMA
  SG-PDL-PHASE
  ))

;;; Fields in sg-state Q
(cl:defvar SG-STATE-FIELD-VALUES '(
  %%SG-ST-CURRENT-STATE 0006 
  %%SG-ST-FOOTHOLD-EXECUTING 0601 
  %%SG-ST-PROCESSING-ERROR 0701
  %%SG-ST-PROCESSING-INTERRRUPT-REQUEST 1001 
  %%SG-ST-SAFE 1101
  %%SG-ST-INST-DISP 1202
  %%SG-ST-IN-SWAPPED-STATE 2601 
  %%SG-ST-SWAP-SV-ON-CALL-OUT 2501 
  %%SG-ST-SWAP-SV-OF-SG-THAT-CALLS-ME 2401
; Set if swapped out sg has saved microstack on special-pdl. Can't use %LP-EXS-MICRO-STACK-SAVED
; because that bit can already be in use by running frame.
  %%SG-ST-MICRO-STACK-SAVED 2301
  ))
(cl:defvar SG-STATE-FIELDS (GET-ALTERNATE SG-STATE-FIELD-VALUES))

(cl:defvar SG-INST-DISPATCHES '(
  SG-MAIN-DISPATCH				;Main instruction dispatch
  SG-DEBUG-DISPATCH				;Debugging dispatch
  SG-SINGLE-STEP-DISPATCH			;Dispatch once, and then break
  SG-SINGLE-STEP-TRAP				;For sequence breaks out of trapping
						; instructions
      ))

(cl:defvar SG-STATES '(
  SG-STATE-ERROR				;0 should never get this
  SG-STATE-ACTIVE				;Actually executing on machine.
  SG-STATE-RESUMABLE				;Reached by interrupt or error recovery
						; completed. Just restore state and do a
						; ucode popj to resume.
  SG-STATE-AWAITING-RETURN			;After doing a "legitimate" sg-call.
						; To resume this, reload SG then return a
						; value by transferring to QMEX1.
  SG-STATE-INVOKE-CALL-ON-RETURN		;To resume this, reload SG, then simulate
						; a store in destination-last. The error
						; system can produce this state when it wants
						; to activate the foothold or perform a retry.
  SG-STATE-INTERRUPTED-DIRTY			;Get this if forced to take an interrupt at an
						; inopportune time.
  SG-STATE-AWAITING-ERROR-RECOVERY		;Immediatedly after error, before recovery
  SG-STATE-AWAITING-CALL
  SG-STATE-AWAITING-INITIAL-CALL
  SG-STATE-EXHAUSTED
  ))

(cl:defvar SPECIAL-PDL-LEADER-QS '(SPECIAL-PDL-SG-HEAD-POINTER))
(cl:defvar REG-PDL-LEADER-QS '(REG-PDL-SG-HEAD-POINTER))

(cl:defvar LENGTH-OF-FASL-TABLE 37773)

(cl:defvar LENGTH-OF-ATOM-HEAD 5)

(cl:defvar SIZE-OF-OB-TBL 177)			;USED BY PRE-PACKAGE INTERN KLUDGE

;;; Size of various hardware memories in "addressible locations"
(cl:defvar SIZE-OF-HARDWARE-CONTROL-MEMORY   40000)
(cl:defvar SIZE-OF-HARDWARE-DISPATCH-MEMORY  4000)
(cl:defvar SIZE-OF-HARDWARE-A-MEMORY         2000)
(cl:defvar SIZE-OF-HARDWARE-M-MEMORY         40) ; #+cadr 40 #+lambda 100 #+explorer 100)
(cl:defvar SIZE-OF-HARDWARE-PDL-BUFFER       2000)
(cl:defvar SIZE-OF-HARDWARE-MICRO-STACK        40)
(cl:defvar SIZE-OF-HARDWARE-LEVEL-1-MAP      4000)
(cl:defvar SIZE-OF-HARDWARE-LEVEL-2-MAP      2000)
(cl:defvar SIZE-OF-HARDWARE-UNIBUS-MAP         20)

(cl:defvar A-MEMORY-LOCATION-NAMES '(	;LIST IN ORDER OF CONTENTS OF A-MEMORY STARTING AT 40
					;  or 100 on lambdas and explorers.
  %MICROCODE-VERSION-NUMBER		;SECOND FILE NAME OF MICROCODE SOURCE FILE AS A NUMBER
  %NUMBER-OF-MICRO-ENTRIES		;NUMBER OF SLOTS USED IN MICRO-CODE-ENTRY-AREA
  DEFAULT-CONS-AREA			;DEFAULT AREA FOR CONS, LIST, ETC.
  NUMBER-CONS-AREA			;FOR BIGNUMS, BIG-FLOATS, ETC.  CAN BE 
					; EXTRA-PDL-AREA OR JUST REGULAR AREA.
  %INITIAL-FEF				;POINTER TO FEF OF FUNCTION MACHINE STARTS UP IN
  %ERROR-HANDLER-STACK-GROUP		;SG TO SWITCH TO ON TRAPS
  %CURRENT-STACK-GROUP			;CURRENT STACK-GROUP
  %INITIAL-STACK-GROUP			;STACK-GROUP MACHINE STARTS UP IN
  %CURRENT-STACK-GROUP-STATE		;SG-STATE Q OF CURRENT STACK GROUP
  %CURRENT-STACK-GROUP-PREVIOUS-STACK-GROUP	;
  %CURRENT-STACK-GROUP-CALLING-ARGS-POINTER	;
  %CURRENT-STACK-GROUP-CALLING-ARGS-NUMBER	;
; %CURRENT-STACK-GROUP-FOLLOWING-STACK-GROUP	;
  %TRAP-MICRO-PC                        ;PC GOTTEN OUT OF OPCS BY TRAP
  %COUNTER-BLOCK-A-MEM-ADDRESS		;LOC OF BEGINNING OF COUNTER BLOCK RELATIVE TO
					; A MEMORY AS A FIXNUM.
  %CHAOS-CSR-ADDRESS			;XBUS ADDRESS
  %MAR-LOW				;FIXNUM MAR LOWER BOUND (INCLUSIVE)
  %MAR-HIGH				;FIXNUM MAR UPPER BOUND (INCLUSIVE)
					;%%M-FLAGS-MAR-MODE CONTROLS THE ABOVE
  SELF					;SELF POINTER FOR DTP-INSTANCE, ETC
  %METHOD-SEARCH-POINTER		;Method list element were last method found.
  INHIBIT-SCHEDULING-FLAG		;NON-NIL SUPPRESSES SEQUENCE BREAKS
  INHIBIT-SCAVENGING-FLAG		;NON-NIL TURNS OFF THE SCAVENGER
  %DISK-RUN-LIGHT			;ADDRESS OF DISK RUN LIGHT, THAT+2 IS PROC RUN LIGHT
  %LOADED-BAND				;LOW 24 BITS (FIXNUM) OF BOOTED BAND NAME (E.G. "OD3")
  %DISK-BLOCKS-PER-TRACK		;(FROM LABEL) BLOCKS PER TRACK, USUALLY 17.
  %DISK-BLOCKS-PER-CYLINDER		;(FROM LABEL) 85. ON T-80, 323. ON T-300
		;THE GARBAGE-COLLECTOR PROCESS HANGS ON THESE VARIABLES
  %REGION-CONS-ALARM			;COUNTS NEW REGIONS CREATED
  %PAGE-CONS-ALARM			;COUNTS PAGES ALLOCATED TO REGIONS
  %GC-FLIP-READY			;If non-NIL, there are no pointers to oldspace
  %INHIBIT-READ-ONLY			;If non-NIL, you can write in read-only
  %SCAVENGER-WS-ENABLE			;If non-NIL, scavenger working set hack enabled
  %METHOD-SUBROUTINE-POINTER		;Continuation point for SELECT-METHOD subroutine
					; or NIL.
  %QLARYH				;Header of last array ref'ed as function
  %QLARYL				;Element # of last array ref'ed as function
  %SCHEDULER-STACK-GROUP		;Force call to this on sequence-break.  This
					;stack group must bind on INHIBIT-SCHEDULING-FLAG as
					;part of the stack-group switch for proper operation.
  %CURRENT-SHEET			;Sheet or screen currently selected by microcode
  %DISK-SWITCHES			;Fixnum: 1 r/c after read, 2 r/c after write
					; 4 enables multiple page swapouts
					; was called %READ-COMPARE-ENABLES
  %MC-CODE-EXIT-VECTOR			;Exit vector used by microcompiled code to ref Q
  					; quantities.
  ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON ;If T, upper and lower case are not equal
  ZUNDERFLOW				;If non-NIL, floating pointer underflow yields zero
  TAIL-RECURSION-FLAG			;Non-NIL says don't save stack frames on tail recursion.
  %METER-GLOBAL-ENABLE			;NIL means metering on per stack group basis
					;T means all stack groups
  %METER-BUFFER-POINTER			;Pointer to the buffer as a fixnum
  %METER-DISK-ADDRESS			;disk address to write out the meter info
  %METER-DISK-COUNT			;count of disk blocks to write out
  CURRENTLY-PREPARED-SHEET		;Error checking for the TV:PREPARE-SHEET macro
  MOUSE-CURSOR-STATE			;0 disabled, 1 open, 2 off, 3 on
  MOUSE-X				;Relative to mouse-sheet
  MOUSE-Y
  MOUSE-CURSOR-X-OFFSET			;From top-left of pattern
  MOUSE-CURSOR-Y-OFFSET			;to the reference point
  MOUSE-CURSOR-WIDTH
  MOUSE-CURSOR-HEIGHT
  MOUSE-X-SPEED				;100ths per second, time averaged
  MOUSE-Y-SPEED				;with time constant of 1/6 second
  MOUSE-BUTTONS-BUFFER-IN-INDEX
  MOUSE-BUTTONS-BUFFER-OUT-INDEX
  MOUSE-WAKEUP				;Set to T when move or click
  LEXICAL-ENVIRONMENT
  AMEM-EVCP-VECTOR			;Value is an array as long as this list plus 40,
					;which holds the EVCP when one of these vars
					;is bound by a closure.
  BACKGROUND-CONS-AREA			;Used for conses that are not explicitly requested
					;and shouldn't go in a temp area.
  SELF-MAPPING-TABLE			;Indirection table mapping ivars of current method's
					;flavor into slots in SELF.
  %GC-SWITCHES
  ARRAY-INDEX-ORDER			;NIL => first array subscript varies fastes.
					;T => last subscript varies fastest.
  PROCESSOR-TYPE-CODE			;1 => CADR, 2 => LAMBDA, 3 => EXPLORER
  AR-1-ARRAY-POINTER-1			;Array whose data is cached for AR-1-CACHED-1.
  AR-1-ARRAY-POINTER-2			;Array whose data is cached for AR-1-CACHED-2.
  ))

(cl:defvar A-MEMORY-COUNTER-BLOCK-NAMES '(
  %COUNT-FIRST-LEVEL-MAP-RELOADS	;# FIRST LEVEL MAP RELOADS
  %COUNT-SECOND-LEVEL-MAP-RELOADS	;# SECOND LEVEL MAP RELOADS
  %COUNT-PDL-BUFFER-READ-FAULTS		;# TOOK PGF AND DID READ FROM PDL-BUFFER
  %COUNT-PDL-BUFFER-WRITE-FAULTS	;# TOOK PGF AND DID WRITE TO PDL-BUFFER
  %COUNT-PDL-BUFFER-MEMORY-FAULTS	;# TOOK PGF FOR PDL-BUF, BUT DATA IN MAIN MEM.
  %COUNT-DISK-PAGE-READS		;COUNT OF PAGES READ FROM DISK
  %COUNT-DISK-PAGE-WRITES		;COUNT OF PAGES WRITTEN TO DISK
  %COUNT-DISK-ERRORS			;COUNT OF RECOVERABLE ERRS
  %COUNT-FRESH-PAGES			;COUNT OF FRESH PAGES 
					; GENERATED IN CORE INSTEAD OF READ FROM DISK
  %COUNT-AGED-PAGES			;NUMBER OF TIMES AGER SET AGE TRAP
  %COUNT-AGE-FLUSHED-PAGES		;NUMBER OF TIMES AGE TRAP -> FLUSHABLE
  %COUNT-DISK-READ-COMPARE-REWRITES	;COUNT OF WRITES REDONE DUE TO FAILURE TO READ-COMPARE
  %COUNT-DISK-RECALIBRATES		;DUE TO SEEK ERRORS
  %COUNT-META-BITS-MAP-RELOADS		;# SECOND LEVEL MAP RELOADS TO META-BITS-ONLY
  %COUNT-CHAOS-TRANSMIT-ABORTS		;Number of transmit aborts in microcode
  %COUNT-DISK-READ-COMPARE-DIFFERENCES	;Number of read-compare differences without
					; accompanying disk read error
  %COUNT-CONS-WORK			;GC parameter
  %COUNT-SCAVENGER-WORK			;..
  %TV-CLOCK-RATE			;TV frame rate divided by this is seq brk clock
  %AGING-DEPTH				;Number of laps to age a page.  Don't make > 3!!
  %COUNT-DISK-ECC-CORRECTED-ERRORS	;Number of soft ECC errors
  %COUNT-FINDCORE-STEPS			;Number of iterations finding mem to swap out
  %COUNT-FINDCORE-EMERGENCIES		;Number of times FINDCORE had to age all pages
  %COUNT-DISK-READ-COMPARE-REREADS	;Reads done over due to r/c diff or error
  %COUNT-DISK-PAGE-READ-OPERATIONS	;Read operations (count once even if multipage)
  %COUNT-DISK-PAGE-WRITE-OPERATIONS	;Write operations (count once even if multipage)
  %COUNT-DISK-PAGE-WRITE-WAITS		;Waiting for a page to get written, to reclaim core
  %COUNT-DISK-PAGE-WRITE-BUSYS		;Waiting for a page to get written, to use disk
  %COUNT-DISK-PREPAGES-USED		;Counts prepaged pages that were wanted
  %COUNT-DISK-PREPAGES-NOT-USED		;Counts prepaged pages that were reclaimed
  %DISK-ERROR-LOG-POINTER		;Address of next 4-word block in 600-637
  %DISK-WAIT-TIME			;Microseconds of waiting for disk time
  %COUNT-DISK-PAGE-WRITE-APPENDS	;Pages appended to swapout operations.
  %COUNT-DISK-PAGE-READ-APPENDS		;Pages appended to swapin operations.
  %LOWEST-DIRECT-VIRTUAL-ADDRESS	;Not a counter (except maybe down, slowly..)
					; Normally equal to LOWEST-A-MEM-VIRTUAL-ADDRESS,
					; set this lower if you need more direct address
					; space, ie, for video buffer of new color display. 
  ;;These two are used to start output on the timestamped output device
  ;;when the interval timer interrupts.
  %UNIBUS-TIMED-OUTPUT-CSR-ADDRESS
  %UNIBUS-TIMED-OUTPUT-CSR-BITS
  %timestamped-output-count-1	;See comments in ucode in UC-PARAMETERS.
  %timestamped-output-count-2

  %count-illop-debug		;Number of times got to ILLOP-DEBUG.  These are ignored unless
				; debug-halts enabled in A-PROCESSOR-SWITCHES.
  %COUNT-MICRO-FAULTS		;Number page-faults in pagable-microcode system.  
  %initial-watchdog				;number of 1/50's of a second the
						;lambda must appear dead before sdu blinks screen
  ))

;;; M-MEM LOCNS ARE ASSIGNED PIECEMEAL..
(cl:defvar M-MEMORY-LOCATION-NAMES '(
  %MODE-FLAGS
  %SEQUENCE-BREAK-SOURCE-ENABLE
  %METER-MICRO-ENABLES
  ))

(cl:setf (cl:get '%MODE-FLAGS 'FORWARDING-VIRTUAL-ADDRESS)
	 (cl:+ A-MEMORY-VIRTUAL-ADDRESS 26))
(cl:setf (cl:get '%SEQUENCE-BREAK-SOURCE-ENABLE 'FORWARDING-VIRTUAL-ADDRESS)
	 (cl:+ A-MEMORY-VIRTUAL-ADDRESS 34))
(cl:setf (cl:get '%METER-MICRO-ENABLES 'FORWARDING-VIRTUAL-ADDRESS)
	 (cl:+ A-MEMORY-VIRTUAL-ADDRESS 35))


(cl:defvar DISK-RQ-LEADER-QS '(
  %DISK-RQ-LEADER-N-HWDS			;Number halfwords really used
						; on first page before CCW list.
  %DISK-RQ-LEADER-N-PAGES			;Number of buffer pages allocated
  %DISK-RQ-LEADER-BUFFER			;Displaced ART-16B array to buffer pgs
  %DISK-RQ-LEADER-8-BIT-BUFFER			;Displaced ART-STRING array.
  ))
(cl:defvar DISK-RQ-HWDS '(
  %DISK-RQ-DONE-FLAG				;0 RQ entered, -1 completed
  %DISK-RQ-DONE-FLAG-HIGH
  ;; These are set up by the requester
  %DISK-RQ-COMMAND				;Disk command register
  %DISK-RQ-COMMAND-HIGH
  %DISK-RQ-CCW-LIST-POINTER-LOW			;CLP low 16
  %DISK-RQ-CCW-LIST-POINTER-HIGH		;CLP high 16
  %DISK-RQ-SURFACE-SECTOR			;Disk address reg low
  %DISK-RQ-UNIT-CYLINDER			;Disk address reg high
  ;; These are stored when the operation completes
  %DISK-RQ-STATUS-LOW				;Disk status reg low 16
  %DISK-RQ-STATUS-HIGH				;Disk status reg high 16
  %DISK-RQ-MEM-ADDRESS-LOW			;Last mem ref addr low 16
  %DISK-RQ-MEM-ADDRESS-HIGH			;Last mem ref addr high 6
  %DISK-RQ-FINAL-SURFACE-SECTOR			;Disk address reg low
  %DISK-RQ-FINAL-UNIT-CYLINDER			;Disk address reg high
  %DISK-RQ-ECC-POSITION
  %DISK-RQ-ECC-PATTERN
  %DISK-RQ-CCW-LIST				;CCW list customarily starts here
  ))
(cl:defvar DISK-HARDWARE-VALUES '(
  %%DISK-STATUS-HIGH-BLOCK-COUNTER 1010
  %%DISK-STATUS-HIGH-INTERNAL-PARITY 0701
  %%DISK-STATUS-HIGH-READ-COMPARE-DIFFERENCE 0601
  %%DISK-STATUS-HIGH-CCW-CYCLE 0501
  %%DISK-STATUS-HIGH-NXM 0401
  %%DISK-STATUS-HIGH-MEM-PARITY 0301
  %%DISK-STATUS-HIGH-HEADER-COMPARE 0201
  %%DISK-STATUS-HIGH-HEADER-ECC 0101
  %%DISK-STATUS-HIGH-ECC-HARD 0001
  ;; Mask for bits which are errors normally
  %DISK-STATUS-HIGH-ERROR 237
  %%DISK-STATUS-LOW-ECC-SOFT 1701
  %%DISK-STATUS-LOW-OVERRUN 1601
  %%DISK-STATUS-LOW-TRANSFER-ABORTED 1501
  %%DISK-STATUS-LOW-START-BLOCK-ERROR 1401
  %%DISK-STATUS-LOW-TIMEOUT 1301
  %%DISK-STATUS-LOW-SEEK-ERROR 1201
  %%DISK-STATUS-LOW-OFF-LINE 1101
  %%DISK-STATUS-LOW-OFF-CYLINDER 1001
  %%DISK-STATUS-LOW-READ-ONLY 0701
  %%DISK-STATUS-LOW-FAULT 0601
  %%DISK-STATUS-LOW-NO-SELECT 0501
  %%DISK-STATUS-LOW-MULTIPLE-SELECT 0401
  %%DISK-STATUS-LOW-INTERRUPT 0301
  %%DISK-STATUS-LOW-SEL-UNIT-ATTENTION 0201
  %%DISK-STATUS-LOW-ATTENTION 0101
  %%DISK-STATUS-LOW-READY 0001
  ;; Mask for bits which are errors normally
  %DISK-STATUS-LOW-ERROR 177560
  %DISK-COMMAND-DONE-INTERRUPT-ENABLE #.(cl:ash 1 11.)
  %DISK-COMMAND-ATTENTION-INTERRUPT-ENABLE #.(cl:ash 1 10.)	;Trident only
  %DISK-COMMAND-RECALIBRATE 10001005
  %DISK-COMMAND-FAULT-CLEAR 10000405		;Recalibrate on Marksman
  %DISK-COMMAND-DATA-STROBE-LATE 200		;These are all different on Marksman
  %DISK-COMMAND-DATA-STROBE-EARLY 100		;..
  %DISK-COMMAND-SERVO-OFFSET 40			;..
  %DISK-COMMAND-SERVO-OFFSET-FORWARD 20		;..
  %DISK-COMMAND-READ 0				;
  %DISK-COMMAND-READ-COMPARE 10
  %DISK-COMMAND-WRITE 11
  %DISK-COMMAND-READ-ALL 2
  %DISK-COMMAND-WRITE-ALL 13
  %DISK-COMMAND-SEEK 20000004
  %%DISK-COMMAND-SEEK-CYLINDER 3010		;Only used by Marksman
  %DISK-COMMAND-AT-EASE 5			;Get status on Marksman
  %DISK-COMMAND-OFFSET-CLEAR 6			;NOP on marksman
  %DISK-COMMAND-RESET-CONTROLLER 16
  ;; Marksman also has get-status commands, not listed here.
  ))
(ASSIGN-VALUES DISK-RQ-LEADER-QS 0)
(ASSIGN-VALUES DISK-RQ-HWDS 0)
(ASSIGN-ALTERNATE DISK-HARDWARE-VALUES)
(cl:defvar DISK-HARDWARE-SYMBOLS (GET-ALTERNATE DISK-HARDWARE-VALUES))

;;; Definitions for interrupt-driven Unibus input channels
;;; Note that these start at 1 rather than at 0, to leave room for an array header

(cl:defvar UNIBUS-CHANNEL-QS '(
  %UNIBUS-CHANNEL-LINK				;Address of next or 0 to end list
  %UNIBUS-CHANNEL-VECTOR-ADDRESS		;Interrupt vector address of device
  %UNIBUS-CHANNEL-CSR-ADDRESS			;Virtual address of status register
  %UNIBUS-CHANNEL-CSR-BITS			;Bits which must be on in CSR
  %UNIBUS-CHANNEL-DATA-ADDRESS			;Virtual address of data register(s)
  %UNIBUS-CHANNEL-BUFFER-START			;Start address of buffer
  %UNIBUS-CHANNEL-BUFFER-END			;End address+1 of buffer
  %UNIBUS-CHANNEL-BUFFER-IN-PTR			;Address of next word to store
  %UNIBUS-CHANNEL-BUFFER-OUT-PTR		;Address of next word to extract
  ;**this last does not really exist now.  It should be carried thru on the next cold load.
  ;  It is required for the non-local unibus hack to work in general, altho we can get along
  ;  without it for the time being since the keyboard is always interrupt enabled.**
  %UNIBUS-CHANNEL-INTERRUPT-ENABLE-BITS		;Bit(s) in CSR which enable interrupts.
  %UNIBUS-CHANNEL-OUTPUT-TURNOFF-ADDRESS	;Address to write to shut down output channel
  %UNIBUS-CHANNEL-OUTPUT-TURNOFF-BITS		;Value to write into that address
  %UNIBUS-CHANNEL-CSR-CLEAR-BITS		;** Bits to clear at start of interrupt.
  %UNIBUS-CHANNEL-CSR-SET-BITS			;** Bits to set at start of interrupt.
  ))
(ASSIGN-VALUES-INIT-DELTA UNIBUS-CHANNEL-QS 0 1 1)

;;; Extra bits in the %UNIBUS-CHANNEL-CSR-BITS word.
;;; Only the bottom 16 bits actually have to do with the device's CSR register
;;; (which is only 16 bits long).
(cl:defvar UNIBUS-CSR-BIT-VALUES '(
  %%UNIBUS-CSR-OUTPUT 2001			;This is an output device.
  %%UNIBUS-CSR-TIMESTAMPED 2101			;Store timestamp with each input char;
						; for output, delay till timestamp is reached.
  %%UNIBUS-CSR-TWO-DATA-REGISTERS 2201		;Device has two 16-bit data registers;
						; assume lower unibus addr has low bits.
  %%UNIBUS-CSR-SB-ENABLE 2301			;Enable sequence break (input only).
  %%UNIBUS-CSR-SET-BITS-P 2401			;** %UNIBUS-CHANNEL-CSR-SET-BITS is
						;  significant.
  %%UNIBUS-CSR-CLEAR-BITS-P 2501		;** %UNIBUS-CHANNEL-CSR-CLEAR-BITS is
						;  significant.
  ))
(ASSIGN-ALTERNATE UNIBUS-CSR-BIT-VALUES)

(cl:defvar UNIBUS-CSR-BITS '(
  %%UNIBUS-CSR-OUTPUT
  %%UNIBUS-CSR-TIMESTAMPED
  %%UNIBUS-CSR-TWO-DATA-REGISTERS
  %%UNIBUS-CSR-SB-ENABLE
  %%UNIBUS-CSR-SET-BITS-P
  %%UNIBUS-CSR-CLEAR-BITS-P
  ))

;;;; Definitions for Chaos net hardware and microcode

;;;  Command/Status register fields

(cl:defvar CHAOS-HARDWARE-VALUES '(
  %%CHAOS-CSR-TIMER-INTERRUPT-ENABLE 0001
  %%CHAOS-CSR-LOOP-BACK 0101
  %%CHAOS-CSR-RECEIVE-ALL 0201
  %%CHAOS-CSR-RECEIVER-CLEAR 0301
  %%CHAOS-CSR-RECEIVE-ENABLE 0401
  %%CHAOS-CSR-TRANSMIT-ENABLE 0501
  %%CHAOS-CSR-INTERRUPT-ENABLES 0402
  %%CHAOS-CSR-TRANSMIT-ABORT 0601
  %%CHAOS-CSR-TRANSMIT-DONE 0701
  %%CHAOS-CSR-TRANSMITTER-CLEAR 1001
  %%CHAOS-CSR-LOST-COUNT 1104
  %%CHAOS-CSR-RESET 1501
  %%CHAOS-CSR-CRC-ERROR 1601
  %%CHAOS-CSR-RECEIVE-DONE 1701
  ;; Offsets of other registers from CSR
  ;; These are in words, not bytes
  %CHAOS-MY-NUMBER-OFFSET 1
  %CHAOS-WRITE-BUFFER-OFFSET 1
  %CHAOS-READ-BUFFER-OFFSET 2
  %CHAOS-BIT-COUNT-OFFSET 3
  %CHAOS-START-TRANSMIT-OFFSET 5
  ))

;;; Leader of a wired Chaos buffer

(cl:defvar CHAOS-BUFFER-LEADER-QS '(
  %CHAOS-LEADER-WORD-COUNT			;Fill pointer for ART-16B array
  %CHAOS-LEADER-THREAD				;Next buffer in wired list (free, rcv, xmt)
						; NIL for end of list
  %CHAOS-LEADER-CSR-1				;Receive stores CSR before reading out here
  %CHAOS-LEADER-CSR-2				;Receive stores CSR after reading out here
						; Get lost-count from here
  %CHAOS-LEADER-BIT-COUNT			;Receive stores bit-count before reading out
  ))
(ASSIGN-VALUES CHAOS-BUFFER-LEADER-QS 0)
(ASSIGN-ALTERNATE CHAOS-HARDWARE-VALUES)
(cl:defvar CHAOS-HARDWARE-SYMBOLS (GET-ALTERNATE CHAOS-HARDWARE-VALUES))

;;;; Ethernet

;;; Offsets from the base of the ether registers to the specific registers
(cl:defvar ether-register-offsets '(
  %ether-output-word-count-offset		;0
  %ether-output-buffer-pointer-offset		;1
  %ether-output-csr-offset 			;2
  %ether-output-delay-offset			;3
  %ether-input-word-count-offset		;4
  %ether-input-buffer-pointer-offset		;5
  %ether-input-csr-offset			;6
  %ether-device-address				;7
  ))
(assign-values ether-register-offsets 0)

;;; Offsets of the leader elements
(cl:defvar ether-leader-offsets '(
  %ether-leader-thread				;0
  %ether-leader-csr				;1
  %ether-leader-active-length			;2
  %ether-leader-transmit-count			;3
  ))
(assign-values ether-leader-offsets 0)

;;; Random parameters
(cl:defvar ether-random-parameters '(
  ether-maximum-packet-length 430		;Max length of packet in words = (// 560. 2)
  ether-unibus-block 0				;Use unibus blocks 0-3
  ))
(assign-alternate ether-random-parameters)

(cl:defvar A-MEMORY-ARRAY-LOCATIONS '(
  MOUSE-CURSOR-PATTERN	1600
  MOUSE-BUTTONS-BUFFER	1640
  MOUSE-X-SCALE-ARRAY	1700
  MOUSE-Y-SCALE-ARRAY	1720
  ))
(cl:defvar A-MEMORY-ARRAY-SYMBOLS (GET-ALTERNATE A-MEMORY-ARRAY-LOCATIONS))


;;; Use of DTP-INSTANCE.  Points to a structure whose header is of
;;; type DTP-INSTANCE-HEADER; the pointer field of that header points
;;; to a structure (generally an array) which contains the fields described
;;; below.  This structure is called an instance-descriptor and contains
;;; the constant or shared part of the instance.  The instance structure,
;;; after its DTP-INSTANCE-HEADER, contains several words used as value
;;; cells of instance variables, which are the variable or unshared
;;; part of the instance.
;;; Note that these are offsets, not indices into the array.  They
;;; are defined here this way because microcode uses them.  This could
;;; be a cdr-coded list or an instance rather than an array.
(cl:defvar INSTANCE-DESCRIPTOR-OFFSETS '(
  %INSTANCE-DESCRIPTOR-HEADER			;The array header.
  %INSTANCE-DESCRIPTOR-RESERVED			;e.g. for named-structure symbol
  %INSTANCE-DESCRIPTOR-SIZE			;The size of the instance; this is one more
						; than the number of instance-variable slots.
						; This is looked at by the garbage collector.
  %INSTANCE-DESCRIPTOR-BINDINGS			;Describes bindings to perform when the
						; instance is called. If this is a list, then
						; SELF is bound to the instance and the
						; elements of the list are locatives to cells
						; which are bound to EVCP's to successive
						; instance-variable slots of the instance. If
						; this is not a list, it is something
						; reserved for future facilities based on the
						; same primitives. NIL is a list.
						; Note that if this is a list, it must be
						; CDR-CODED! The microcode depends on this for
						; a little extra speed.
  %INSTANCE-DESCRIPTOR-FUNCTION			;Function to be called when the instance
						; is called.  Typically a hash table.
  %INSTANCE-DESCRIPTOR-TYPENAME			;A symbol which is returned by TYPEP
  %INSTANCE-DESCRIPTOR-MAPPING-TABLE-ALIST	;Mapping tables to instances of this descr
						; for various method-flavors.
  %INSTANCE-DESCRIPTOR-IGNORE			;Used only at higher levels
  %INSTANCE-DESCRIPTOR-ALL-INSTANCE-VARIABLES	;List of all instance variables.
  %INSTANCE-DESCRIPTOR-IGNORE
  %INSTANCE-DESCRIPTOR-IGNORE
  %INSTANCE-DESCRIPTOR-IGNORE
  %INSTANCE-DESCRIPTOR-IGNORE
  %INSTANCE-DESCRIPTOR-IGNORE
  %INSTANCE-DESCRIPTOR-DEPENDS-ON-ALL		;List of all component flavors names.
						; For TYPEP-STRUCTURE-OR-FLAVOR.
  ;; Additional slots may exist, defined by the particular class system employed.
  ;; If the instance-descriptor is an array, it must not be so long as to
  ;; contain a long-length Q.
  ))
(ASSIGN-VALUES INSTANCE-DESCRIPTOR-OFFSETS 0)

(cl:defvar METER-ENABLES-VALUES '(
  %%METER-PAGE-FAULT-ENABLE 0001		;Page fault metering
  %%METER-CONS-ENABLE 0101			;Cons metering
  %%METER-FUNCTION-ENTRY-EXIT-ENABLE 0201	;Function call metering
  %%METER-STACK-GROUP-SWITCH-ENABLE 0301	;Stack group metering
  %%METER-MACRO-INSTRUCTION-ENABLE 0401		;Macro-instruction metering
  ))
(cl:defvar METER-EVENTS '(
  %METER-PAGE-IN-EVENT
  %METER-PAGE-OUT-EVENT
  %METER-CONS-EVENT
  %METER-FUNCTION-ENTRY-EVENT
  %METER-FUNCTION-EXIT-EVENT
  %METER-FUNCTION-UNWIND-EVENT
  %METER-STACK-GROUP-SWITCH-EVENT
  %METER-MACRO-INSTRUCTION-EVENT
  ))
(ASSIGN-ALTERNATE METER-ENABLES-VALUES)
(cl:defvar METER-ENABLES (GET-ALTERNATE METER-ENABLES-VALUES))
(ASSIGN-VALUES METER-EVENTS 0 1)

(cl:DEFUN ASSIGN-QCOM-VALUES ()
  (ASSIGN-VALUES ADI-KINDS 0)
  (ASSIGN-VALUES ADI-STORING-OPTIONS 0)
  (ASSIGN-ALTERNATE ARG-DESC-FIELD-VALUES)
  (ASSIGN-ALTERNATE ARRAY-FIELD-VALUES)
  (ASSIGN-ALTERNATE ARRAY-LEADER-FIELD-VALUES)
  (ASSIGN-ALTERNATE ARRAY-MISC-VALUES)
  (ASSIGN-VALUES ARRAY-TYPES 19.)
  (ASSIGN-VALUES FEF-ARG-SYNTAX 4)
  (ASSIGN-VALUES FEF-DES-DT 11)
  (ASSIGN-VALUES FEF-FUNCTIONAL 15)
  (ASSIGN-VALUES FEF-INIT-OPTION 0)
  (ASSIGN-VALUES FEF-NAME-PRESENT 20)
  (ASSIGN-VALUES FEF-QUOTE-STATUS 7)
  (ASSIGN-VALUES FEF-SPECIALNESS 16)
  (ASSIGN-VALUES FEFHI-INDEXES 0)
  (ASSIGN-ALTERNATE FEFHI-VALUES)
  (ASSIGN-ALTERNATE HEADER-FIELD-VALUES)
  (ASSIGN-VALUES Q-CDR-CODES 0)
  (ASSIGN-VALUES Q-DATA-TYPES 0)
  (ASSIGN-VALUES Q-HEADER-TYPES 0)
  (ASSIGN-ALTERNATE SG-STATE-FIELD-VALUES)
  (ASSIGN-VALUES SG-STATES 0)
  (ASSIGN-VALUES SG-INST-DISPATCHES 0)
  (ASSIGN-VALUES SPECIAL-PDL-LEADER-QS 0)
  (ASSIGN-VALUES STACK-GROUP-HEAD-LEADER-QS 0)
  (ASSIGN-VALUES SYSTEM-COMMUNICATION-AREA-QS 0)
  (ASSIGN-VALUES REG-PDL-LEADER-QS 0))

(ASSIGN-QCOM-VALUES)			;Foo.  ASSIGN-VALUES, etc had better be defined.
