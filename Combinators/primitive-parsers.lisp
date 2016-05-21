(cl:in-package #:nomenclatura-combinators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser class IDENTITY.

(defclass identity (parser)
  ((%value :initarg :value :reader value)))

(defun identity (value)
  (make-instance 'identity :value value))

(defmethod parse ((parser identity) input)
  (list (cons (value parser) input)))
