(in-package #:dbus-bind)

(defmacro define-dbus-method (name signature (path interface destination) &body options)
  ;; doesn't really work with non-atomic types, because I don't understand them or how DBUS uses them.
  "Define a D-Bus method proxy.

NAME is the (Lisp symbol) name of the method.
PATH, INTERFACE, and DESTINATION are the items of the same names for D-Bus.
SIGNATURE is a D-Bus method signature - both lispy and string verions work.
OPTIONS is a list of other options, in standard (:keyword ...) format.  Only the first instance of an option is used.
  :documentation attaches a docstring to the defined function.
  :arg-names provides names for the arguments.
  :error-class should be the name of a condition class.
    It's used as the error signalled if a generated method is called with no bus available.
  :bus-name is the name of the special variable the generated method looks for a bus in.
    If not provided, the method will just give up.

The defined function will have an arglist of (,@ARGS &OPTIONAL [gensym]), where the gensym is a connection.
If this optional argument is not provided, *BUS* will be tried instead.
If *BUS* is nil, an error (of the type controlled with the :error-class option, or just DBUS's default) is signalled."
  (alexandria:with-gensyms (bus)
    (let ((documentation (rest (assoc :documentation options)))
	  (arg-names (or (rest (assoc :arg-names options)) (mapcar (alexandria:compose
								    #'gensym
								    #'dbus-bind.sys:signature->string) (sigexp signature))))
	  (errclass (second (assoc :error-class options)))
	  (bus-var (second (assoc :bus-name options))))
      `(defun ,name (,@arg-names &optional ,bus)
	 ,@documentation
	 (let ((,bus (or ,bus ,bus-var ,@(when errclass (list `(error ',errclass :method ',name))))))
	   (invoke-method (bus-connection ,bus)
			  ,(dbus-bind.sys:lisp->dbus-name name)
			  :path ,path
			  :interface ,interface
			  :destination ,destination
			  :signature ,(dbus:signature signature)
			  :arguments (list ,@arg-names)))))))