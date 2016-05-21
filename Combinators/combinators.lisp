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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser class SATISFIES.

(defclass satisfies (parser)
  ((%predicate :initarg :predicate :reader predicate)
   (%arguments :initarg :arguments :reader arguments)))

(defun satisfies (predicate &rest arguments)
  (make-instance 'satisfies
    :predicate predicate
    :arguments arguments))

(defmethod parse ((parser satisfies) input)
  (bind (item) 
	(lambda (x) 
	  (if (apply (predicate parser) x (arguments parser))
	      (identity x)
	      (fail)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser class IS-NOT.

(defclass is-not (parser)
  ((%predicate :initarg :predicate :reader predicate)
   (%arguments :initarg :arguments :reader arguments)))

(defun is-not (predicate &rest arguments)
  (make-instance 'is-not
    :predicate predicate
    :arguments arguments))

(defmethod parse ((parser is-not) input)
  (satisfies (lambda (i) 
	       (not (apply (predicate parser) i (arguments parser))))))
