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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser class FAIL.

(defclass fail (parser)
  ())

(defun fail ()
  (make-instance 'fail))

(defmethod parse ((parser fail) input)
  '())
