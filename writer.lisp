(in-package #:dbus-bind)

(defun write-interface-package (interface &key nicknames exports)
  "Write an interface's package-defining form.
EXPORTS is prepended to the interface's list of methods."
  (write (macroexpand-1 `(define-interface-package ,(interface-name interface) (,@nicknames)
			   (,@exports ,@(interface-exports (list-interface-methods interface)))))))

(defun write-in-package-form (interface)
  "Write an interface's in-package form."
  (let ((*package* (find-package :keyword))) ; I'm certain there's a better way to force the CL prefix to be printed
    (write (macroexpand-1 `(in-interface-package ,(interface-name interface))))))

(defun write-bus-definer-form (bus-name interface-name)
  "Write the form that defines an interface's special bus variable."
  (write (macroexpand-1 `(define-bus ,bus-name ,interface-name))))

(defun write-with-macro (bus-name macro-name &key system)
  "Write the form that defines the WITH-[name] macro."
  (write (macroexpand-1 `(define-with-macro ,bus-name ,macro-name :system ,system))))

(defun write-condition-definition (condition-name condition-slot-name)
  "Write the definition of the condition signaled by a method proxy when no bus is available to it."
  (write (macroexpand-1 `(define-buslessness-condition ,condition-name ,condition-slot-name))))

(defun write-interface-macro (path destination interface
				    &key name definer-options (interface-human-name (interface-name interface)))
  "Write the definition of a shortcut macro for defining method proxies.
Note that this depends on DEFINE-DBUS-MACRO being available to macroexpand."
  (let ((definer (or name (alexandria:symbolicate '#:define- interface-human-name '#:-method))))
    (write (macroexpand-1 `(define-interface-macro ,definer (,path ,(interface-name interface) ,destination)
			     ,@definer-options)))))

(defun write-interface-definitions (path destination interface &rest options)
  "Write all of the method proxy definitions for the given interface."
  (dolist (method (list-interface-methods interface))
    (fresh-line)
    (write (macroexpand-1 `(define-dbus-method ,(make-symbol (dbus-bind.sys:dbus-name->lisp (method-name method)))
			       ,(method-signature method) (,path ,(interface-name interface) ,destination) ,@options)))))

(defun write-interface-proxy-from-object (object interface shortname &key system)
  (write-interface-proxy (object-path object) (object-destination object) (object-interface interface object)
			 shortname :system system))

(defun write-interface-proxy (path destination interface name &key system)
  "Write all the necessary forms to define an interface proxy."
  (let ((*package* (find-package :keyword)))
    (write-in-package-form interface) (fresh-line))
  (let ((*print-gensym* nil) (*print-escape* t))
    (write-bus-definer-form '#:*bus* name) (fresh-line)
    (write-with-macro '#:*bus* (make-symbol (concatenate 'string (symbol-name '#:with-) (string-upcase name))) :system system)
    (fresh-line)
    (write-condition-definition '#:no-bus-error '#:no-bus-error-method) (fresh-line)
    (write-interface-definitions path destination interface '(:error-class #:no-bus-error) '(:bus-name #:*bus*))))

(defun write-interface-proxies-from-name (path destination interface name &key system)
  "Convenience wrapper around write-interface-file, so you don't need to put in the path/dest twice."
  (write-interface-file path destination (get-interface path destination interface :system system) name :system system))

(defun write-object-proxies (path destination name &key system)
  "Write all the interface proxies for a given object."
  (dolist (interface (list-object-interfaces (make-proxy-object path destination :system system)))
    (write-interface-file path destination interface name :system system)
    (terpri)))

(defun grovel-interface (path destination interface name &key system package-filename package-nicknames interface-filename)
  (let ((object (make-proxy-object path destination :system system)))
    (with-open-file (*standard-output* package-filename :if-does-not-exist :create :direction :output)
      (write-interface-package (object-interface interface object)
			       :nicknames package-nicknames
			       :exports (list (make-symbol (concatenate 'string (symbol-name '#:with-) (string-upcase name)))
					      '#:no-bus-error '#:no-bus-error-method)))
    (with-open-file (*standard-output* interface-filename :if-does-not-exist :create :direction :output)
      (write-interface-proxy path destination (object-interface interface object) name :system system))
    (values)))