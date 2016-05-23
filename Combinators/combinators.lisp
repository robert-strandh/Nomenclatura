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
  (parse (bind (item)
	       (lambda (x)
		 (if (apply (predicate parser) x (arguments parser))
		     (identity x)
		     (fail))))
	 input))

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
  (parse (satisfies (lambda (i)
		      (not (apply (predicate parser) i (arguments parser)))))
	 input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser class PLUS.

(defclass plus (parser)
  ((%parser1 :initarg :parser1 :reader parser1)
   (%parser2 :initarg :parser2 :reader parser2)))

(defun plus (parser1 parser2)
  (make-instance 'plus
    :parser1 parser1
    :parser2 parser2))

(defmethod parse ((parser plus) input)
  (append (parse (parser1 parser) input)
	  (parse (parser2 parser) input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser class CONS.

(defclass cons (parser)
  ((%object :initarg :object :reader object)
   (%parser :initarg :parser :reader parser)))

(defun cons (object parser)
  (make-instance 'cons
    :object object
    :parser parser))

(defmethod parse ((parser cons) input)
  (parse (bind (parser parser)
	       (lambda (result)
		 (identity (cl:cons (object parser) result))))
	 input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser class ZERO-OR-MORE.

(defclass zero-or-more (parser)
  ((%parser :initarg :parser :reader parser)))

(defun zero-or-more (parser)
  (make-instance 'zero-or-more
    :parser parser))

(defmethod parse ((parser zero-or-more) input)
  (cl:cons '()
	   (parse (bind (parser parser)
			(lambda (result)
			  (cl:cons result (zero-or-more (parser parser)))))
		  input)))
