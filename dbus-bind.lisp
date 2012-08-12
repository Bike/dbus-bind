;;;; dbus-bind.lisp

(in-package #:dbus-bind)

;; dbus: already has object-interface, list-object-interfaces, interface-method, list-interface-methods

(defmacro define-interface-macro (macro-name (path interface destination) &body options)
  "Define a macro that specifies define-dbus-method for a given interface."
  (alexandria:with-gensyms (name signature inner-options)
    `(defmacro ,macro-name (,name ,signature &body ,inner-options)
      `(define-dbus-method ,,name ,,signature (,,path ,,interface ,,destination)
	 ,@(append ,inner-options ',options)))))

(defun generate-interface-proxy-definitions (interface macro-name)
  "Given an interface and a defining macro for it, retuns a list of definitions."
  (mapcar (lambda (method)
	    `(,macro-name ,(make-symbol (dbus-name->lisp (method-name method))) ,(method-signature method)))
	  (list-interface-methods interface)))

(defmacro define-buslessness-condition (condition-name condition-slot-name)
  "Define a condition signaled when no bus is provided."
  (alexandria:with-gensyms (cond stream)
    `(define-condition ,condition-name (error)
       ((,condition-slot-name :accessor ,condition-slot-name :initarg :method))
       (:documentation "Error signalled if a D-Bus method proxy is called when no bus is available to it.")
       (:report (lambda (,cond ,stream)
		  (format ,stream "~a was called without a D-Bus bus available." (,condition-slot-name ,cond)))))))

(defmacro define-bus (bus-name program-name)
  "Define the special variable used as the default bus."
  `(defvar ,bus-name nil ,(format nil "The default bus for ~a methods." program-name)))

(defmacro define-with-macro (bus-name macro-name &key system)
  "Define a macro that defines a simple wrapper around DBUS:WITH-OPEN-BUS."
  (alexandria:with-gensyms (body)
    `(defmacro ,macro-name (&body ,body)
       `(if ,',bus-name
	    ,@,body ; don't expand recursively
	    (with-open-bus (,',bus-name ,',(if system '(dbus:system-server-addresses) '(dbus:session-server-addresses)))
	      ,@,body)))))

(defun interface-name->symbol (name)
  "Get a symbol corresponding to an interface name."
  ;; TODO: Is this necessary to worry about?  If so, should it conditionalize on read-table case or something?
  (make-symbol (string-upcase name)))

;;; Possible change: put exports in separate (CL:EXPORT) forms, separate the actual interface exports
(defmacro define-interface-package (interface-name (&rest nicknames) (&rest exports)
				    &optional (package-name (interface-name->symbol interface-name)))
  "Define an interface's package."
  `(defpackage ,package-name
     (:documentation
      ,(format nil "Automatically generated bindings for the ~a D-Bus interface.
See DBUS-BIND and ~:*~a's own documentation for more information." interface-name))
     (:use #:cl #:dbus)
     (:nicknames ,@nicknames)
     (:export ,@exports)))

(defmacro in-interface-package (interface-name)
  "In-package an interface's package, simply enough."
  `(in-package ,(interface-name->symbol interface-name)))

(defun interface-exports (methods)
  "Given a list of methods, returns an export list suitable to DEFPACKAGE."
  (mapcar (lambda (method)
	    (make-symbol (dbus-bind.sys:dbus-name->lisp (method-name method))))
	  methods))
