(cl:in-package #:asdf-user)

(defsystem :nomenclatura-combinators-test
  :depends-on (:nomenclatura-combinators)
  :serial t
  :components
  ((:file "packages")
   (:file "test")))
