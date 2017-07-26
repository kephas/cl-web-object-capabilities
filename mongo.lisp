(defpackage :web-object-capabilities/mongo
  (:use :cl :alexandria :cl-mongo :scheme :metabang-bind)
  (:import-from :web-object-capabilities/key #:new-key-string)
  (:import-from :closer-mop #:class-slots #:slot-definition-name)
  (:export #:create-key! #:find-document #:set-document-value! #:first-time? #:create-first-time-key! #:store-config! #:get-config #:get-element
		   #:serialize #:unserialize
		   #:write-object #:read-object #:remake-object))

(in-package :web-object-capabilities/mongo)


(defparameter +admin+ "ADMIN")
(defparameter +keys+ "keys")


(defun create-key! (type &rest kvs)
  (let ((key (new-key-string)))
	(db.insert +keys+ (apply #'kv (kv "key" key) (kv "type" type) kvs))
	key))

(defun find-document (key)
  (first (docs (db.find +keys+ (kv (kv "key" key))))))

(defun set-document-value! (key/doc name value)
  (if-let (doc (if (stringp key/doc)
				   (find-document key/doc)
				   key/doc))
	(progn
	  (add-element name value doc)
	  (db.save +keys+ doc))))


(defun first-time? ()
  (not (docs (db.find +keys+ (kv "type" +admin+)))))

(defun create-first-time-key! ()
  "Create the first admin key or return NIL if already created"
  (when (first-time?)
	(create-key! +admin+)))


(defun store-config! (name value)
  (if-let (results (docs (db.find "config" (kv "name" name))))
	  (db.delete "config" results))
  (db.insert "config" (kv (kv "name" name) (kv "value" value))))

(defun get-config (name)
  (get-element "value" (first (docs (db.find "config" (kv "name" name))))))


#| Very crude serialization functions |#

(defgeneric serialize (object))

(defmethod serialize ((object string)) object)
(defmethod serialize ((object number)) object)
(defmethod serialize ((object (eql t))) object) ; (object boolean) didn't work
(defmethod serialize ((object (eql nil))) object)


(defgeneric unserialize (parent slot object))

(defmethod unserialize (parent slot (object string)) (declare (ignore parent slot)) object)
(defmethod unserialize (parent slot (object number)) (declare (ignore parent slot)) object)
(defmethod unserialize (parent slot (object (eql t))) (declare (ignore parent slot)) object)
(defmethod unserialize (parent slot (object (eql nil))) (declare (ignore parent slot)) object)


(defun write-object (object key fields)
  (let ((root (find-document key)))
	(let@ rec ((document root)
			   (fields fields))
	  (if fields
		  (let ((subdoc (get-element (first fields) document)))
			(unless subdoc
			  (setf subdoc (make-document))
			  (add-element (first fields) subdoc document))
			(rec subdoc (rest fields)))
		  (progn
			(dolist (slot (mapcar #'slot-definition-name (class-slots (class-of object))))
			  (when (slot-boundp object slot)
				(add-element (string slot) (serialize (slot-value object slot)) document)))
			(db.save +keys+ root))))))

(defun read-object (object key fields)
  (let ((root (find-document key)))
	(let@ rec ((document root)
			   (fields fields))
	  (if fields
		  (rec (get-element (first fields) document) (rest fields))
		  (dolist (slot (mapcar #'slot-definition-name (class-slots (class-of object))))
			(bind (((:values value found?) (get-element (string slot) document)))
			  (when found?
				(setf (slot-value object slot) (unserialize object slot value)))))))))

(defun remake-object (class key fields)
  (let ((object (make-instance class)))
	(read-object object key fields)
	object))
