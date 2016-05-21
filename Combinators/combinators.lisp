(cl:in-package #:nomenclatura-combinators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser class BIND.

(defclass bind (parser)
  ((%parser :initarg :parser :reader parser)
   (%function :initarg :function :reader function)))

(defun bind (parser function)
  (make-instance 'bind
    :parser parser
    :function function))

(defmethod parse ((parser bind) input)
  (loop for (result . rest) in (parse (parser parser) input)
	append (parse (funcall (function parser) result) rest)))
