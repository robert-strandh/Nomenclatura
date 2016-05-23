(cl:in-package #:nomenclatura-combinators-test)

(defmethod nclc:input-empty-p (input)
  (endp input))

(defmethod nclc:input-first (input)
  (first input))

(defmethod nclc:input-rest (input)
  (rest input))
