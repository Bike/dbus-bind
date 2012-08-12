;;;; package.lisp

(defpackage #:dbus-bind
  (:use #:cl #:dbus)
  (:export
   ;; primitives (wrappers around dbus, or reexports)
   #:make-proxy-object
   #:get-interface
   #:get-interface-methods
   ;; reexports
   #:object-interface
   #:list-object-interfaces
   #:interface-method
   #:list-interface-methods
   ;; for using dbus-bind "manually"
   #:define-dbus-method ; move to .sys
   ;; code generators
   #:define-interface-macro
   #:define-interface-proxy
   #:generate-interface-proxy-definitions
   #:define-buslessness-condition
   #:define-bus
   #:define-interface-package
   #:in-interface-package
   ;; writers
   #:write-interface-package
   #:write-in-package-form
   #:write-bus-definer-form
   #:write-with-macro
   #:write-condition-definition
   #:write-interface-macro
   #:write-interface-definitions
   #:write-interface-proxy
   #:write-interface-proxy-from-name
   #:write-object-proxies))

(defpackage #:dbus-bind.sys
  (:use :cl :dbus)
  (:export
   #:lisp->dbus-name
   #:dbus-name->lisp
   #:signature->string
   #:dbus-type-of
   #:arg-typer))