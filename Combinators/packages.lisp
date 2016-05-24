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
   #:parse
   #:identity
   #:fail
   #:item
   #:bind
   #:satisfies
   #:is-not
   #:plus
   #:cons
   #:zero-or-more
   #:one-or-more))
