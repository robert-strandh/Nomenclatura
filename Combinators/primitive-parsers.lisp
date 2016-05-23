(cl:in-package #:nomenclatura-combinators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser class IDENTITY.

(defclass identity (parser)
  ((%value :initarg :value :reader value)))

(defun identity (value)
  (make-instance 'identity :value value))

(defmethod parse ((parser identity) input)
  (list (cl:cons (value parser) input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser class FAIL.

(defclass fail (parser)
  ())

(defun fail ()
  (make-instance 'fail))

(defmethod parse ((parser fail) input)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser class ITEM.

(defclass item (parser)
  ())

(defun item ()
  (make-instance 'item))

(defmethod parse ((parser item) input)
  (if (input-empty-p input)
      '()
      (list (cl:cons (input-first input)
		     (input-rest input)))))
