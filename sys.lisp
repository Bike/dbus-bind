(in-package #:dbus-bind.sys)

(defun signature->string (signature &optional (plural 1))
  "Return a verbose string corresponding to a D-Bus method signature."
  (etypecase signature
    (symbol (format nil "~a~p" (symbol-name signature) plural))
    (list (ecase (first signature)
	    ((:array) (format nil "~:@(array~p-of-~a~)" plural (signature->string (second signature) 2)))
	    ((:struct) (format nil "~:@(struct~p-of-(~{~a~^,~})~)" plural (mapcar #'signature->string (rest signature))))
	    ((:dict-entry) (format nil "~:@(dict-entry~)"))))))

(defun lisp->dbus-name (symbol)
  "Return a CamelCase name corresponding to a Lisp symbol's name."
  (remove #\- (string-capitalize symbol) :test #'char=))
(defun dbus-name->lisp (string)
  "Return a Lisp symbol corresponding to a CamelCase name."
  ;; doesn't work well, e.g. GetNetworkESSID -> get-network-e-s-s-i-d.  CamelCase is hard.
  (string-upcase (subseq (ppcre:regex-replace-all "([A-Z])" string "-\\1") 1)))

(defun dbus-type-of (thing)
  "Guess the D-Bus type of a provided Lisp thingie.  Gives up on anything remotely complicated.
Intended for use with D-Bus variant signatures."
  (etypecase thing
    (boolean :boolean)
    ((unsigned-byte 8) :byte)
    ((signed-byte 16) :int16)
    ((unsigned-byte 16) :uint16)
    ((signed-byte 32) :int32)
    ((unsigned-byte 32) :uint32)
    ((signed-byte 64) :int64)
    ((unsigned-byte 64) :uint64)
    (double-float :double)
    (string :string)))

(defun arg-typer (sig arg)
  "Will possibly be used in variant type inference in define-dbus-method."
  ;; FIXME: Ugly, doesn't support most types
  (let ((sig (if (characterp sig) (dbus:sigexp (string sig)) sig)))
    (if (eq (first sig):variant)
	(if (listp arg)
	    `(list ,(dbus:signature (list (first arg))) ,(second arg))
	    `(list ,(dbus-type-of arg) ,arg))
	arg)))

#||
(defparameter *ascii-alphanumeric*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_"
  "Alphanumeric characters in ASCII, for use of valid-object-path-p.")
(defun valid-object-path-p (string)
  (unless (stringp string) (return-from valid-object-path-p nil))
  (unless (char= (char string 0) #\/) (return-from valid-object-path-p nil))
  (when (= (length string) 1) (return-from valid-object-path-p (char= (char string 0) #\/)))
  (dotimes (n (length string) t)
    (cond ((char= (char string n) #\/)
	   (when (or (= (1- (length string)) n)
		     (char= (char string (1+ n)) #\/))
	     (return nil)))
	  ((find (char string n) *ascii-alphanumeric*))
	  (t (return nil)))))
(deftype dbus-object-path () `(and string (satisfies valid-object-path-p)))
||#