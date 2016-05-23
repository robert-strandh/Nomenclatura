(cl:in-package #:nomenclatura-combinators-test)

(defmethod nclc:input-empty-p (input)
  (endp input))

(defmethod nclc:input-first (input)
  (first input))

(defmethod nclc:input-rest (input)
  (rest input))


(defun test-identity ()
  (assert (equal (nclc:parse (nclc:identity 234) '())
		 '((234))))
  (assert (equal (nclc:parse (nclc:identity 234) '(a b c))
		 '((234 a b c)))))

(defun test-fail ()
  (assert (equal (nclc:parse (nclc:fail) '())
		 '()))
  (assert (equal (nclc:parse (nclc:fail) '(a b c))
		 '())))

(defun test-satisfies ()
  (assert (equal (nclc:parse (nclc:satisfies #'symbolp) '())
		 '()))
  (assert (equal (nclc:parse (nclc:satisfies #'symbolp) '(234 a b))
		 '()))
  (assert (equal (nclc:parse (nclc:satisfies #'symbolp) '(a b c))
		 '((a b c)))))

(defun test-is-not ()
  (assert (equal (nclc:parse (nclc:is-not #'symbolp) '())
		 '()))
  (assert (equal (nclc:parse (nclc:is-not #'symbolp) '(234 a b))
		 '((234 a b))))
  (assert (equal (nclc:parse (nclc:is-not #'symbolp) '(a b c))
		 '())))

(defun test ()
  (test-identity)
  (test-fail)
  (test-satisfies))
