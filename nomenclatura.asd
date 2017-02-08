(cl:in-package #:asdf-user)

(defsystem :nomenclatura
  :depends-on (:nomenclatura-combinators)
  :description "Library for creating parsers."
  :author "Robert Strandh <robert.strandh@gmail.com>"
  :license "FreeBSD, see file LICENSE.text")
