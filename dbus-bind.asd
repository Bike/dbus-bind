;;;; dbus-bind.asd

(asdf:defsystem #:dbus-bind
  :serial t
  :description "A binding groveler for D-Bus using its introspection capabilities."
  :author "Bike <aeshtaer@gmail.com>"
  :license "WTFPL"
  :depends-on (#:dbus #:cl-ppcre #:alexandria)
  :components ((:file "package")
	       (:file "primitives")
	       (:file "sys")
	       (:file "layer")
               (:file "dbus-bind")
	       (:file "writer")))