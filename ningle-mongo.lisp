(uiop:define-package :web-object-capabilities/ningle-mongo
  (:use :cl :alexandria :ningle :web-object-capabilities/mongo)
  (:reexport :web-object-capabilities/mongo)
  (:export #:lambda-with-key #:param #:make-key-handler #:handle-document))

(in-package :web-object-capabilities/ningle-mongo)


(defmacro lambda-with-key ((~key key-param ~document) error &body body)
  (with-gensyms (~params)
	`(lambda (,~params)
	   (labels ((param (name) (assoc-value ,~params name :test 'equal)))
		 (let ((,~key (param ,key-param)))
		   (if-let (,~document (find-document ,~key))
			 (progn
			   ,@body)
			 (progn
			   (setf (lack.response:response-status *response*) 404)
			   ,error)))))))

(defmacro make-key-handler (key-param (&rest handle-args) error)
  `(lambda-with-key (key ,key-param document) ,error
	 (handle-document (intern (get-element "type" document) :keyword) document ,@handle-args)))


(defgeneric handle-document (type document &key &allow-other-keys))

(defmethod handle-document (type document &key &allow-other-keys)
  (declare (ignore type document))
  '(400 (:content-type "text/plain;") ("No such key")))
