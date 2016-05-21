(cl:in-package #:asdf-user)

(defsystem :nomenclatura-combinators
  :serial t
  :components
  ((:file "packages")
   (:file "input")
   (:file "parse")
   (:file "parser")
   (:file "primitive-parsers")
   (:file "combinators")))
