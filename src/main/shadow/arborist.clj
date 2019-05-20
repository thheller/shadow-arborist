(ns shadow.arborist
  (:require [shadow.arborist.fragments :as fragments]))

(defmacro << [& body]
  (fragments/make-fragment &env body))

(defmacro fragment [& body]
  (fragments/make-fragment &env body))