A set of functions for groveling bindings to D-Bus interfaces via its introspection mechanisms.

Example usage:

;;; Say we want to use the notification mechanism, org.freedesktop.Notifications.  First we check the interfaces with DBUS:

(use-package :dbus)
(with-open-bus (bus (session-server-addresses))
	   (list-object-interfaces
	    (make-object-from-introspection (bus-connection bus)
					    "/org/freedesktop/Notifications" "org.freedesktop.Notifications")))
=> (#<DBUS::INTERFACE "org.freedesktop.Notifications">
 #<DBUS::INTERFACE "org.freedesktop.DBus.Properties">
 #<DBUS::INTERFACE "org.freedesktop.DBus.Introspectable">)

We want notifications, so:


