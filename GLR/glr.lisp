;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; grammar

(defclass rule ()
  ((left-hand-side :initarg :left-hand-side :reader left-hand-side)
   (right-hand-side :initarg :right-hand-side :reader right-hand-side)
   (builder :initarg :builder :reader builder)))

(defmacro grammar-rule ((left-hand-side arrow arglist &body body))
  (declare (ignore arrow))
  (flet ((var-of (arg)
	   (if (symbolp arg) arg (car arg)))
	 (sym-of (arg)
	   (if (symbolp arg) arg (cadr arg))))
    `(make-instance 'rule
	:left-hand-side ',left-hand-side
        :right-hand-side ',(mapcar #'sym-of arglist)
	:builder
         (lambda ,(mapcar #'var-of arglist)
	   (declare (ignorable ,@(mapcar #'var-of arglist)))
	   ,(if (or (null body)
		    (symbolp (car body)))
		`(make-instance ',left-hand-side ,@body)
		`(progn ,@body))))))

(defmethod print-object ((obj rule) stream)
  (format stream "[~a -> ~a]"
	  (left-hand-side obj)
	  (right-hand-side obj)))

(defclass grammar ()
  ((start :initarg :start :reader start)
   (artificial-start :reader artificial-start)
   (start-state :reader start-state)
   (rules :initform '() :accessor rules)
   (states :initarg :states :accessor states)))

(defmacro add-rule (rule grammar)
  `(push (grammar-rule ,rule) (rules ,grammar)))

;;; an item is represented as a cons of a rule and an 
;;; integer i, such that 0 <= i <= len, where len is the 
;;; length of the right hand side of the rule

(defun make-item (rule pos) (cons rule pos))
(defun item-rule (item) (car item))
(defun item-pos (item) (cdr item))

(defun item-symbol (item)
  (unless (terminal-item-p item)
    (elt (right-hand-side (item-rule item)) (item-pos item))))

(defun terminal-item-p (item)
  (= (item-pos item) (length (right-hand-side (item-rule item)))))

(defun epsilon-terminal-item-p (item)
  (zerop (length (right-hand-side (item-rule item)))))

(defun item-equal (item1 item2)
  (and (eq (car item1) (car item2))
       (eql (cdr item1) (cdr item2))))
  
(defun item-next (item)
  (make-item (item-rule item) (1+ (item-pos item))))

(defun closure (item-set grammar)
  (let ((result item-set)
	(remaining item-set))
    (loop until (null remaining)
	  do (let* ((item (pop remaining)))
	       (unless (terminal-item-p item)
		 (let ((symbol (item-symbol item)))
		   (loop for rule in (rules grammar)
			 do (when (eq symbol (left-hand-side rule))
			      (let ((item (make-item rule 0)))
				(unless (member item result :test #'item-equal)
				  (push item result)
				  (pushnew item remaining :test #'item-equal)))))))))
    result))

(defclass state ()
  ((id :initform (gensym) :reader id)
   (items :initarg :items :reader items)))

(defclass lr0-state (state)
  ((transitions :initform '() :accessor transitions)))

(defmethod print-object ((obj lr0-state) stream)
  (format stream "{~a LR0: ~a ~a}~%"
	  (id obj)
	  (items obj)
	  (mapcar (lambda (trans) (cons (car trans) (id (cdr trans))))
		  (transitions obj))))

(defclass shift-state (state)
  ((transitions :initarg :transitions :reader transitions)))

(defmethod print-object ((obj shift-state) stream)
  (format stream "{~a SH: ~a ~a}~%"
	  (id obj)
	  (items obj)
	  (mapcar #'(lambda (trans) (cons (car trans) (id (cdr trans))))
		  (transitions obj))))

(defclass terminal-state (state) ())

(defclass normal-terminal-state (terminal-state) ())

(defmethod print-object ((obj normal-terminal-state) stream)
  (format stream "{~a NT: ~a}~%"
	  (id obj) (car (items obj))))

(defclass epsilon-terminal-state (terminal-state)
  ((transition :initarg :transition :reader transition)))

(defmethod print-object ((obj epsilon-terminal-state) stream)
  (format stream "{~a ET: ~a ~a}~%"
	  (id obj) (car (items obj)) (id (transition obj))))

(defclass split-state (state)
  ((successors :initarg :successors :reader successors)))

(defclass final-state (normal-terminal-state) ())

(defmethod print-object ((obj final-state) stream)
  (format stream "{~a FI: ~a}~%"
	  (id obj) (car (items obj))))

(defmethod print-object ((obj split-state) stream)
  (format stream "{~a SP: ~a ~a}~%"
	  (id obj) (items obj) (mapcar #'id (successors obj))))

(defun state-equal (state1 state2)
  (null (set-exclusive-or (items state1) (items state2)
			  :test #'equal)))

(defun state-next (state grammar symbol)
  (make-instance 'lr0-state
    :items (closure (mapcar #'item-next
			    (remove symbol (items state)
				    :key #'item-symbol
				    :test-not #'eq))
		    grammar)))

(defun transition-symbols (state)
  (remove-duplicates (mapcar #'item-symbol
			     (remove-if #'terminal-item-p (items state)))
		     :test #'eq))

(defun compute-states (grammar)
  (let* ((states (list (start-state grammar)))
	 (remaining states))
    (loop until (null remaining)
	  do (let* ((state (pop remaining))
		    (symbols (transition-symbols state)))
	       (loop for symbol in symbols
		     do (let* ((next-state (state-next state grammar symbol))
			       (existing (find next-state states
					       :test #'state-equal)))
			  (if existing
			      (setf next-state existing)
			      (progn (push next-state states)
				     (push next-state remaining)))
			  (push (cons symbol next-state)
				(transitions state))))))
    (setf (states grammar) states)))
			  
;;; dont't try to find an existing state
(defun make-new-shift-state (items transitions grammar)
  (let ((new (make-instance 'shift-state
			    :items items
			    :transitions transitions)))
    (push new (states grammar))
    new))

;;; dont't try to find an existing state
(defun make-new-epsilon-terminal-states (items transitions grammar)
  (mapcar (lambda (item)
	    (let ((new (make-instance 'epsilon-terminal-state
				      :items (list item)
				      :transition (cdar (member (left-hand-side (item-rule item))
								transitions
								:key #'car :test #'eq)))))
	      (push new (states grammar))
	      new))
	  items))

(defun make-new-normal-terminal-states (items grammar)
  (mapcar (lambda (item)
	    (let* ((new (make-instance (if (eq (left-hand-side (item-rule item))
					       (artificial-start grammar))
					   'final-state
					   'normal-terminal-state)
				       :items (list item)))
		   (existing (find new (states grammar) :test #'state-equal)))
	      (if (null existing)
		  (progn (push new (states grammar))
			 new)
		  existing)))
	  items))

(defun possibly-split-state (state grammar)
  (let* ((terminal-items (remove-if-not #'terminal-item-p (items state)))
	 (epsilon-terminal-items (remove-if-not #'epsilon-terminal-item-p terminal-items))
	 (normal-terminal-items (remove-if #'epsilon-terminal-item-p terminal-items))
	 (shift-items (remove-if #'terminal-item-p (items state))))
    (cond ((and (not (null shift-items))
		(not (null terminal-items)))
	   (let ((new-shift-state (make-new-shift-state shift-items (transitions state) grammar))
		 (new-epsilon-terminal-states
		  (make-new-epsilon-terminal-states epsilon-terminal-items (transitions state) grammar))
		 (new-normal-terminal-states
		  (make-new-normal-terminal-states normal-terminal-items grammar)))
	     (let ((all (cons new-shift-state
			      (append new-epsilon-terminal-states new-normal-terminal-states))))
	       (change-class state 'split-state :successors all))))
	  ((not (null (cdr terminal-items)))
	   (let ((new-epsilon-terminal-states
		  (make-new-epsilon-terminal-states epsilon-terminal-items (transitions state) grammar))
		 (new-normal-terminal-states
		  (make-new-normal-terminal-states normal-terminal-items grammar)))
	     (let ((all (append new-epsilon-terminal-states new-normal-terminal-states)))
	       (change-class state 'split-state :successors all))))
	  ((not (null shift-items))
	   (change-class state 'shift-state))
	  ((eq (left-hand-side (item-rule (car (items state))))
	       (artificial-start grammar))
	   (change-class state 'final-state))
	  (t
	   (change-class state 'normal-terminal-state)))))

(defun split-states (grammar)
  (loop with states = (states grammar)
	for state in states
	do (possibly-split-state state grammar)))
	       
(defun preprocess-grammar (grammar)
  (with-slots (start-state artificial-start) grammar
    (let* ((unique (gensym))
	   (start-rule (make-instance 'rule
				      :left-hand-side unique
				      :right-hand-side (list (start grammar))))
	   (items (closure (list (make-item start-rule 0)) grammar)))
      (setf artificial-start unique
	    start-state (make-instance 'lr0-state :items items))))
  (compute-states grammar)
  (split-states grammar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; lexer

(defclass lexer ()
  ((position :initform 0 :initarg :position :accessor lex-position)))

(defclass list-lexer (lexer)
  ((contents :initarg :contents :accessor contents)))

(defgeneric clone-lexer (lexer))
(defgeneric lex (lexer state))
(defgeneric lex-eof (lexer))

(defmethod clone-lexer ((lexer list-lexer))
  (make-instance 'list-lexer
    :contents (contents lexer)
    :position (lex-position lexer)))

(defmethod lex ((lexer list-lexer) state)
  (declare (ignore state))
  (incf (lex-position lexer))
  (pop (contents lexer)))

(defmethod lex-eof ((lexer list-lexer))
  (null (contents lexer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; parser

(defclass parse-tree ()
  ((symbol :initarg :symbol :reader head-symbol)
   (children :initarg :children :reader children)))

(defmethod print-object ((obj parse-tree) stream)
  (format stream "(~a " (head-symbol obj))
  (format stream "~a" (children obj))
  (format stream ")"))

(defclass stack-entry ()
  ((state :initarg :state :reader state)
   (parse-tree :initform nil :initarg :parse-tree :reader parse-tree)
   (next :initarg :next :reader next)))

(defclass task ()
  ((stacks :initform '() :initarg :stacks :accessor stacks)
   (state :initarg :state :accessor state)
   (lexer :initarg :lexer :accessor lexer)))

(defun clone-task (task)
  (make-instance 'task
    :stacks (stacks task)
    :state (state task)
    :lexer (clone-lexer (lexer task))))

(defclass parser ()
  ((grammar :initarg :grammar :reader grammar)
   (result :initform nil :accessor result)
   (tasks :accessor tasks)))

(defmethod initialize-instance :after ((parser parser) &rest args &key lexer)
  (declare (ignore args))
  (preprocess-grammar (grammar parser))
  (with-slots (tasks) parser
    (let* ((task (make-instance 'task
		   :state (start-state (grammar parser))
		   :lexer lexer)))
      (setf tasks (list task)))))

(defgeneric advance-parse-task (task parser state))

(defmethod advance-parse-task (task parser (state final-state))
  (when (lex-eof (lexer task))
    (setf (result parser) (parse-tree (car (stacks task))))))

(defun push-stack (task parse-tree state)
  (setf (stacks task)
	(list (make-instance 'stack-entry
		:state (state task)
		:parse-tree parse-tree
		:next (stacks task))))
  (setf (state task) state))

(defun task< (task1 task2)
  (< (lex-position (lexer task1)) (lex-position (lexer task2))))

(defun task= (task1 task2)
  (and (= (lex-position (lexer task1)) (lex-position (lexer task2)))
       (eq (state task1) (state task2))))

(defun insert-task (task list)
  (if (or (null list) (task< task (car list)))
      (push task list)
      (loop for l on list
	    do (cond ((task= task (car l))
		      (setf (stacks (car l))
			    (nconc (stacks (car l)) (stacks task)))
		      (return))
		     ((or (null (cdr l))
			  (task< task (cadr l)))
		      (push task (cdr l))
		      (return))
		     (t nil))))
  list)

(defun add-task (task parser)
  (setf (tasks parser) (insert-task task (tasks parser))))

(defun pop-and-transit (parser state stacks lexer symbol parse-trees n)
  (if (zerop n)
      (let* ((parse-tree (make-instance 'parse-tree
		  	   :symbol symbol
			   :children parse-trees))
	     (new-state (cdr (find symbol (transitions state)
				   :key #'car :test #'eq)))
	     (new-stack-entry (make-instance 'stack-entry
			        :state state
				:parse-tree parse-tree
				:next stacks))
	     (new-task (make-instance 'task
			 :stacks (list new-stack-entry)
			 :state new-state
			 :lexer (clone-lexer lexer))))
	(add-task new-task parser))
      (loop for stack in stacks
	    do (pop-and-transit parser
				(state stack)
				(next stack)
				lexer
				symbol
				(cons (parse-tree stack) parse-trees)
				(1- n)))))

(defmethod advance-parse-task (task parser (state shift-state))
  (let* ((lexeme (lex (lexer task) state))
	 (transition (find lexeme (transitions state) :key #'car :test #'eq)))
    (when transition
      (push-stack task lexeme (cdr transition))
      (add-task task parser))))

(defmethod advance-parse-task (task parser (state split-state))
  (loop for new-state in (successors state)
	do (let ((new-task (clone-task task)))
	     (setf (state new-task) new-state)
	     (add-task new-task parser))))

(defmethod advance-parse-task (task parser (state normal-terminal-state))
  (let ((symbol (left-hand-side (item-rule (car (items state)))))
	(n (length (right-hand-side (item-rule (car (items state)))))))
    (pop-and-transit parser (state task) (stacks task) (lexer task) symbol nil n)))

(defmethod advance-parse-task (task parser (state epsilon-terminal-state))
  (let ((symbol (left-hand-side (item-rule (car (items state))))))
    (let ((parse-tree (make-instance 'parse-tree
			:symbol symbol
			:children '()))
	  (state (transition state)))
      (push-stack task parse-tree state)
      (add-task task parser))))

(defun advance-parse (parser task)
  (advance-parse-task task parser (state task)))

(defun parse (parser)
  (loop until (null (tasks parser))
	do (advance-parse parser (pop (tasks parser))))
  (result parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; test

;;; ETF

(defparameter *etf* (make-instance 'grammar :start 'E))

(add-rule (expr -> (expr + term)) *etf*)
(add-rule (expr -> (term)) *etf*)
(add-rule (term -> (term * factor)) *etf*)
(add-rule (term -> (factor)) *etf*)
(add-rule (factor -> (a)) *etf*)

(defparameter *etf-lexer*
  (make-instance 'list-lexer :contents '(A + A * A)))

(defparameter *etf-parser*
  (make-instance 'parser :grammar *etf* :lexer *etf-lexer*))

;;; List

(defparameter *list* (make-instance 'grammar :start 'S))

(add-rule (S -> ([ A ])) *list*)
(add-rule (A -> ()) *list*)
(add-rule (A -> (A b)) *list*)


;;; Tomita's example

(defparameter *tomita* (make-instance 'grammar :start 'S))

(add-rule (S -> (NP VP)) *tomita*)
(add-rule (S -> (S PP)) *tomita*)
(add-rule (NP -> (n)) *tomita*)
(add-rule (NP -> (det n)) *tomita*)
(add-rule (NP -> (NP PP)) *tomita*)
(add-rule (PP -> (prep NP)) *tomita*)
(add-rule (VP -> (v NP)) *tomita*)

(defparameter *tomita-lexer*
  (make-instance 'list-lexer
    :contents '(n v det n
		prep det n
		prep det n
		prep det n
		prep det n
		prep det n
		prep det n
		prep det n
		prep det n)))

(defparameter *tomita-parser*
    (make-instance 'parser :grammar *tomita* :lexer *tomita-lexer*))


