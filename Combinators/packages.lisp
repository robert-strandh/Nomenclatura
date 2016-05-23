(cl:in-package #:common-lisp-user)

(defpackage #:nomenclatura-combinators
  (:nicknames #:nclc)
  (:use #:common-lisp)
  (:shadow #:identity
	   #:function
	   #:satisfies
	   #:cons)
  (:export
   #:input-empty-p
   #:input-first
   #:input-rest
   #:identity
   #:fail
   #:item))
