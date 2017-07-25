(defpackage :web-object-capabilities/mongo
  (:use :cl :alexandria :cl-mongo)
  (:import-from :web-object-capabilities/key #:new-key-string)
  (:export #:create-key! #:find-document #:set-document-value! #:first-time? #:create-first-time-key! #:store-config! #:get-config))

(in-package :web-object-capabilities/mongo)


(defparameter +admin+ "ADMIN")


(defun create-key! (type &rest kvs)
  (let ((key (new-key-string)))
	(db.insert "keys" (apply #'kv (kv "key" key) (kv "type" type) kvs))
	key))

(defun find-document (key)
  (first (docs (db.find "keys" (kv (kv "key" key))))))

(defun set-document-value! (key name value)
  (if-let (doc (find-document key))
	(progn
	  (add-element name value doc)
	  (db.save "keys" doc))))


(defun first-time? ()
  (not (docs (db.find "keys" (kv "type" +admin+)))))

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
