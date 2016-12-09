(cl:in-package #:nomenclatura-earley)

(defclass lr0-item ()
  ((%symbol :initarg :symbol :reader symbol)))

(defclass internal-lr0-item (lr0-item)
  ((%next :initarg :next :reader next)))

(defclass scanner-lr0-item (internal-lr0-item)
  ())

(defclass predictor-lr0-item (internal-lr0-item)
  ((grammar :initarg :grammar :reader item-grammar)))

(defclass completer-lr0-item (item)
  ())

(defclass grammar ()
  ((%rules :initarg :items
	   :initform (make-hash-table :test #'eq)
	   :reader rules)))

(defun grammar-rules (grammar symbol)
  (gethash symbol (rules grammar)))

(defmacro grammar (&rest rules)
  (let ((nonterminals (mapcar #'car rules)))
    (labels ((make-item (symbol symbols)
	       (cond ((null symbols)
		      `(make-instance 'completer-lr0-item
			              :symbol ',symbol))
		     ((member (car symbols) nonterminals)
		      `(make-instance 'predictor-lr0-item
			              :symbol ',(car symbols)
			              :grammar grammar
			              :next ,(make-item symbol (cdr symbols))))
		     (t
		      `(make-instance 'scanner-lr0-item
			              :symbol ',(car symbols)
			              :next ,(make-item symbol (cdr symbols)))))))
      `(let* ((grammar (make-instance 'grammar))
	      (items (rules grammar)))
	 ,@(mapcar (lambda (rule)
		     `(push ,(make-item (car rule) (cddr rule))
		            (gethash ',(car rule) items)))
		   rules)
	 grammar))))						  

(defclass earley-item ()
  ((%lr0-item :initarg :item :reader lr0-item)
   (%state :initarg :state :reader state)))

(defun compare-earley-item (lr0-item state earley-item)
  (and (eq lr0-item (lr0-item earley-item))
       (eq state (state earley-item))))

(defclass parser-state ()
  ((%predictor-earley-items :initform '() :accessor predictor-earley-items)
   (5scanner-earley-items :initform '() :accessor scanner-earley-items)))

(defgeneric earley-item-in-state-p (lr0-item state state))

(defmethod earley-item-in-state-p ((lr0-item scanner-lr0-item) state state)
  (member-if (lambda (earley-item)
	       (compare-earley-item lr0-item state earley-item))
	     (scanner-earley-items state)))

(defmethod earley-item-in-state-p ((lr0-item predictor-lr0-item) state state)
  (member-if (lambda (earley-item)
	       (compare-earley-item lr0-item state earley-item))
	     (predictor-earley-items state)))

(defgeneric add-earley-item-to-state (lr0-item state state))

(defmethod add-earley-item-to-state ((lr0-item scanner-lr0-item) state state)
  (push (make-instance 'earley-item :item lr0-item :state state)
	(scanner-earley-items state)))

(defmethod add-earley-item-to-state ((lr0-item predictor-lr0-item) state state)
  (push (make-instance 'earley-item :item lr0-item :state state)
	(predictor-earley-items state)))

(defun predict (symbol grammar state)
  (loop for item in (grammar-rules grammar symbol)
	do (process-earley-item item state state)))

(defgeneric process-earley-item (lr0-item state state))

(defmethod process-earley-item ((lr0-item scanner-lr0-item) state state)
  (unless (earley-item-in-state-p lr0-item state state)
    (add-earley-item-to-state lr0-item state state)))

(defmethod process-earley-item ((lr0-item predictor-lr0-item) state state)
  (unless (earley-item-in-state-p lr0-item state state)
    (add-earley-item-to-state lr0-item state state)
    (predict (symbol lr0-item) (item-grammar lr0-item) state)))
  
(defmethod process-earley-item ((lr0-item completer-lr0-item) state state)
  (loop with symbol = (symbol lr0-item)
	for earley-item in (predictor-earley-items state)
	do (when (eq symbol (symbol (lr0-item earley-item)))
	     (process-earley-item (next (lr0-item earley-item))
			     (state earley-item)
			     state))))

(defun parse (grammar symbol stream)
  (let* ((initial-state (make-instance 'parser-state))
	 (state initial-state)
	 (terminator (gensym))
	 (item (make-instance
		'predictor-lr0-item
		:symbol symbol
		:grammar grammar
		:next (make-instance
		       'scanner-lr0-item
		       :symbol terminator
		       :next nil))))
    (process-earley-item item initial-state initial-state)
    (loop while stream
	  do (let ((new-state (make-instance 'parser-state))
		   (token (pop stream)))
	       (loop for earley-item in (scanner-earley-items state) do
		     (when (eq token (symbol (lr0-item earley-item)))
		       (process-earley-item (next (lr0-item earley-item))
				       (state earley-item)
				       new-state)))
	       ;; (setf (scanner-earley-items state) nil)
	       (setf state new-state)))
    (print (if (member terminator
		       (scanner-earley-items state)
		       :key (lambda (s) (symbol (lr0-item s)))
		       :test #'eq)
	       'YES
	       'NO))))

(defparameter *g* (grammar (E -> T) (E -> E + T) (T -> F) (T -> T * F) (F -> V)))
