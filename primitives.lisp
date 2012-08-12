(in-package #:dbus-bind)

(defun make-proxy-object (path destination &key system)
  ;; is this actually necessary?
  (with-open-bus (bus (if system (system-server-addresses) (session-server-addresses)))
    (make-object-from-introspection (bus-connection bus) path destination)))

(defun get-interface (path destination interface &key system)
  (object-interface interface (make-proxy-object path destination :system system)))

(defun get-interface-methods (path destination interface &key system)
  (list-interface-methods (get-interface path destination interface :system system)))
