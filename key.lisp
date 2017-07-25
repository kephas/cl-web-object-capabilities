(defpackage :web-object-capabilities/key
  (:use :cl :cl-base64 :uuid)
  (:export #:new-key-string))

(in-package :web-object-capabilities/key)


(defun new-key-string ()
  (usb8-array-to-base64-string (uuid-to-byte-array (make-v4-uuid)) :uri t))
