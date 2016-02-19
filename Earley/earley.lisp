(cl:in-package #:nomenclatura-earley)

(defclass item ()
  ((symbol :initarg :symbol :reader item-symbol)))

(defclass internal-item (item)
  ((next :initarg :next :reader item-next)))

(defclass scanner-item (internal-item)
  ())

(defclass predictor-item (internal-item)
  ((grammar :initarg :grammar :reader item-grammar)))

(defclass completer-item (item)
  ())

(defclass grammar ()
  ((items :initarg :items :initform (make-hash-table :test #'eq) :reader grammar-items)))

(defun grammar-rules (grammar symbol)
  (gethash symbol (grammar-items grammar)))

(defmacro grammar (&rest rules)
  (let ((nonterminals (mapcar #'car rules)))
    (labels ((make-item (symbol symbols)
	       (cond ((null symbols)
		      `(make-instance 'completer-item
			              :symbol ',symbol))
		     ((member (car symbols) nonterminals)
		      `(make-instance 'predictor-item
			              :symbol ',(car symbols)
			              :grammar grammar
			              :next ,(make-item symbol (cdr symbols))))
		     (t
		      `(make-instance 'scanner-item
			              :symbol ',(car symbols)
			              :next ,(make-item symbol (cdr symbols)))))))
      `(let* ((grammar (make-instance 'grammar))
	      (items (grammar-items grammar)))
	 ,@(mapcar (lambda (rule)
		     `(push ,(make-item (car rule) (cddr rule))
		            (gethash ',(car rule) items)))
		   rules)
	 grammar))))						  

(defclass suffix ()
  ((item :initarg :item :reader suffix-item)
   (state :initarg :state :reader suffix-state)))

(defun compare-suffix (suffix-item suffix-state suffix)
  (and (eq suffix-item (suffix-item suffix))
       (eq suffix-state (suffix-state suffix))))

(defclass parser-state ()
  ((predictor-suffixes :initform '() :accessor predictor-suffixes)
   (scanner-suffixes :initform '() :accessor scanner-suffixes)))

(defgeneric suffix-in-state-p (suffix-item suffix-state state))

(defmethod suffix-in-state-p ((suffix-item scanner-item) suffix-state state)
  (member-if (lambda (suffix)
	       (compare-suffix suffix-item suffix-state suffix))
	     (scanner-suffixes state)))

(defmethod suffix-in-state-p ((suffix-item predictor-item) suffix-state state)
  (member-if (lambda (suffix)
	       (compare-suffix suffix-item suffix-state suffix))
	     (predictor-suffixes state)))

(defgeneric add-suffix-to-state (suffix-item suffix-state state))

(defmethod add-suffix-to-state ((suffix-item scanner-item) suffix-state state)
  (push (make-instance 'suffix :item suffix-item :state suffix-state)
	(scanner-suffixes state)))

(defmethod add-suffix-to-state ((suffix-item predictor-item) suffix-state state)
  (push (make-instance 'suffix :item suffix-item :state suffix-state)
	(predictor-suffixes state)))

(defun predict (symbol grammar state)
  (loop for item in (grammar-rules grammar symbol)
	do (process-suffix item state state)))

(defgeneric process-suffix (suffix-item suffix-state state))

(defmethod process-suffix ((suffix-item scanner-item) suffix-state state)
  (unless (suffix-in-state-p suffix-item suffix-state state)
    (add-suffix-to-state suffix-item suffix-state state)))

(defmethod process-suffix ((suffix-item predictor-item) suffix-state state)
  (unless (suffix-in-state-p suffix-item suffix-state state)
    (add-suffix-to-state suffix-item suffix-state state)
    (predict (item-symbol suffix-item) (item-grammar suffix-item) state)))
  
(defmethod process-suffix ((suffix-item completer-item) suffix-state state)
  (loop with symbol = (item-symbol suffix-item)
	for suffix in (predictor-suffixes suffix-state)
	do (when (eq symbol (item-symbol (suffix-item suffix)))
	     (process-suffix (item-next (suffix-item suffix))
			     (suffix-state suffix)
			     state))))

(defun parse (grammar symbol stream)
  (let* ((initial-state (make-instance 'parser-state))
	 (state initial-state)
	 (terminator (gensym))
	 (item (make-instance
		'predictor-item
		:symbol symbol
		:grammar grammar
		:next (make-instance
		       'scanner-item
		       :symbol terminator
		       :next nil))))
    (process-suffix item initial-state initial-state)
    (loop while stream
	  do (let ((new-state (make-instance 'parser-state))
		   (token (pop stream)))
	       (loop for suffix in (scanner-suffixes state) do
		     (when (eq token (item-symbol (suffix-item suffix)))
		       (process-suffix (item-next (suffix-item suffix))
				       (suffix-state suffix)
				       new-state)))
	       ;; (setf (scanner-suffixes state) nil)
	       (setf state new-state)))
    (print (if (member terminator
		       (scanner-suffixes state)
		       :key (lambda (s) (item-symbol (suffix-item s)))
		       :test #'eq)
	       'YES
	       'NO))))

(defparameter *g* (grammar (E -> T) (E -> E + T) (T -> F) (T -> T * F) (F -> V)))
