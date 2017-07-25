(defpackage :web-object-capabilities-system
  (:use :common-lisp :asdf))

(in-package :web-object-capabilities-system)

(defsystem "web-object-capabilities"
  :description "Web-exposed object capabilities framework"
  :version "0.1"
  :author "Pierre Thierry <pierre@nothos.net>"
  :class :package-inferred-system
  :licence "MIT")
