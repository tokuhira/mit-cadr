;;; -*- Mode:LISP; Readtable:T; Base:8; Lowercase:T; Package: cold -*-
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;This package is the program

(defpackage :cold (:use :cl)
  (:export #:assign-alternate #:get-alternate
	   #:assign-values #:assign-values-init-delta
	   #:make-cold
	   #:vprint-q
	   #:defmic
	   #:load-parameters
	   #:defprop))

;;This package is used to contain symbols which stand for symbols in the cold-load
;;being built; this includes all the system data structure definition symbols.

(defpackage :cold-symbols (:nicknames :sym)
  (:use :cold)
  (:import-from :common-lisp t nil)
  (:export #:page-size
	   #:dtp-trap #:dtp-null #:dtp-free
	   #:dtp-symbol #:dtp-symbol-header
	   #:dtp-fix #:dtp-extended-number #:dtp-header #:dtp-gc-forward
	   #:dtp-external-value-cell-pointer #:dtp-one-q-forward
	   #:dtp-header-forward #:dtp-body-forward #:dtp-locative #:dtp-list
	   #:dtp-u-entry
	   #:dtp-fef-pointer #:dtp-array-pointer #:dtp-array-header
	   #:dtp-stack-group #:dtp-closure #:dtp-small-flonum
	   #:dtp-select-method #:dtp-instance #:dtp-instance-header
	   #:dtp-entity #:dtp-stack-closure #:dtp-self-ref-pointer
	   #:dtp-character
	   #:%%q-data-type
	   #:%%q-cdr-code
	   #:%%q-all-but-typed-pointer
	   #:%%q-all-but-pointer
	   #:%%q-pointer
	   #:%%q-typed-pointer
	   #:cdr-normal #:cdr-error #:cdr-nil #:cdr-next
	   #:q-data-types #:q-cdr-codes
	   #:q-corresponding-variable-lists
	   #:%%array-type-field #:%%array-long-length-flag
	   #:%%array-displaced-bit #:%%array-leader-bit
	   #:%%array-index-length-if-short
	   #:%array-max-short-index-length
	   #:%%array-number-dimensions
	   #:array-dim-mult #:array-types
	   #:art-1b #:art-8b #:art-16b #:art-32b
	   #:art-q #:art-q-list #:art-string #:art-stack-group-head
	   #:art-special-pdl #:art-reg-pdl
	   #:system-constant-lists
	   #:cold-load-area-sizes
	   #:cold-load-region-sizes
	   #:scratch-pad-pointers
	   #:scratch-pad-parameters
	   #:scratch-pad-parameter-offset
	   #:support-vector-contents
	   #:constants-page
	   #:read-only-area-list
	   #:wired-area-list
	   #:pdl-buffer-area-list
	   #:list-structured-areas
	   #:static-areas
	   #:area-name #:region-origin #:region-length
	   #:region-free-pointer #:region-gc-pointer #:region-bits
	   #:area-region-list #:area-region-size
	   #:area-maximum-size #:region-list-thread
	   #:micro-code-entry-area
	   #:micro-code-entry-name-area
	   #:micro-code-entry-args-info-area
	   #:micro-code-entry-arglist-area
	   #:micro-code-entry-max-pdl-usage
	   #:micro-code-symbol-area
	   #:micro-code-symbol-name-area
	   #:support-entry-vector
	   #:constants-area
	   #:system-communication-area
	   #:page-table-area
	   #:area-list
	   #:init-list-area
	   #:working-storage-area
	   #:extra-pdl-area
	   #:scratch-pad-init-area
	   #:default-cons-area
	   #:linear-pdl-area #:linear-bind-pdl-area
	   #:property-list-area
	   #:resident-symbol-area
	   #:fasl-symbol-head-area
	   #:fasl-symbol-string-area
	   #:fasl-array-area
	   #:fasl-frame-area
	   #:fasl-list-area
	   #:fasl-temp-list-area
	   #:fasl-temp-area
	   #:a-memory-array-locations
	   #:a-memory-location-names
	   #:a-memory-virtual-address
	   #:m-memory-location-names
	   #:forwarding-virtual-address
	   #:%gc-generation-number
	   #:%sys-com-gc-generation-number
	   #:new-array-index-order
	   #:prin1 #:base #:ibase #:*print-base* #:*read-base*
	   #:*print-radix*
	   #:*nopoint #:for-cadr
	   #:lambda-list-keywords
	   #:qintcmp #:qlval
	   #:&optional #:&rest #:&aux #:&special #:&local #:&functional #:&eval
	   #:&quote #:&quote-dontcare #:&dt-dontcare #:&dt-number #:&dt-fixnum
	   #:&dt-symbol #:&dt-atom #:&dt-list #:&dt-frame #:&function-cell
	   #:&list-of #:&body #:&key #:&allow-other-keys
	   #:%address-space-quantum-size
	   #:%%region-map-bits
	   #:%%region-oldspace-meta-bit
	   #:%%region-extra-pdl-meta-bit
	   #:%%region-representation-type
	   #:%region-space-extra-pdl
	   #:%region-space-fixed
	   #:%region-space-static
	   #:%region-space-new
	   #:%%region-space-type
	   #:%%region-scavenge-enable
	   #:physical-page-data
	   #:address-space-map
	   #:%address-space-map-byte-size
	   #:p-n-string #:nr-sym
	   #:length-of-atom-head
	   #:array-elements-per-q
	   #:array-bits-per-element
	   #:%header-type-error #:%header-type-fef #:%header-type-array-leader
	   #:%header-type-flonum #:%header-type-complex #:%header-type-bignum
	   #:%header-type-rational-bignum
	   #:%%header-type-field #:%%array-leader-length
	   #:array-leader-bit
	   #:array-named-structure-flag
	   #:array-displaced-bit
	   #:array-long-length-flag
	   #:function #:quote #:*catch #:argdesc #:t #:nil
	   #:control-tables #:obarray
	   #:fef-arg-rest #:fef-arg-req #:fef-arg-opt
	   #:fef-qt-eval #:fef-qt-dontcare
	   #:%arg-desc-quoted-rest #:%arg-desc-evaled-rest
	   #:%arg-desc-fef-quote-hair
	   #:%%fefh-pc-in-words
	   #:%fefhi-storage-length  #:%fefhi-fctn-name
	   #:%instance-descriptor-header #:%instance-descriptor-reserved
	   #:%instance-descriptor-size
	   #:initial-top-level-function #:lisp-top-level
	   #:current-stack-group #:initial-stack-group
	   #:error-handler-stack-group #:main-stack-group
	   #:sg-state-active #:sg-state #:sg-name
	   #:sg-regular-pdl #:sg-special-pdl
	   #:sg-regular-pdl-limit #:sg-special-pdl-limit
	   #:stack-group-head-leader-qs
	   #:reg-pdl-leader-qs #:special-pdl-leader-qs
	   #:special-pdl-sg-head-pointer #:reg-pdl-sg-head-pointer
	   #:sg-initial-function-index
	   #:%sys-com-area-origin-pntr
	   #:%sys-com-valid-size
	   #:%sys-com-page-table-pntr
	   #:%sys-com-page-table-size
	   #:%sys-com-obarray-pntr
	   #:%sys-com-ether-free-list
	   #:%sys-com-ether-transmit-list
	   #:%sys-com-ether-receive-list
	   #:%sys-com-band-format
	   #:%sys-com-gc-generation-number
	   #:%sys-com-unibus-interrupt-list
	   #:%sys-com-temporary
	   #:%sys-com-free-area#-list
	   #:%sys-com-free-region#-list
	   #:%sys-com-memory-size
	   #:%sys-com-wired-size
	   #:%sys-com-chaos-free-list
	   #:%sys-com-chaos-transmit-list
	   #:%sys-com-chaos-receive-list
	   #:%sys-com-debugger-requests
	   #:%sys-com-debugger-keep-alive
	   #:%sys-com-debugger-data-1
	   #:%sys-com-debugger-data-2
	   #:%sys-com-major-version
	   #:%sys-com-desired-microcode-version
	   #:%sys-com-highest-virtual-address
	   #:%sys-com-pointer-width
	   #:system-communication-area-qs
	   #:active-micro-code-entries
	   #:size-of-area-arrays
	   #:setq #:and #:or #:cond #:macro #:fset #:lambda #:set
	   #:lisp-crash-list
	   #:mouse-cursor-pattern #:mouse-buttons-buffer
	   #:mouse-x-scale-array #:mouse-y-scale-array
	   #:definitions
	   #:*cold-loaded-file-property-lists*
	   #:file-id-package-alist
	   #:fasl-table-working-offset
	   #:length-of-fasl-table
	   #:macro-compiled-program
	   #:fasl-ops
	   #:%fasl-group-check
	   #:%fasl-group-flag
	   #:%%fasl-group-length
	   #:%fasl-group-type
	   #:fasl-evaled-value
	   #:si #:system-internals #:system #:sys #:global
	   #:cold-load-function-property-lists
	   #:unspecific #:|:UNSPECIFIC|
	   #:newest #:|:NEWEST|
	   #:record-source-file-name #:|:SOURCE-FILE-NAME|
	   #:|:PROPERTY| #:|:INTERNAL| #:|:INTERNAL-FEF-OFFSETS|
	   #:|FS:MAKE-PATHNAME-INTERNAL|
	   #:|FS:MAKE-FASLOAD-PATHNAME|
	   #:deff #:forward-value-cell
	   #:defvar-1 #:defconst-1
	   ))

;(fset 'cold-symbols:logdpb #'dpb)


