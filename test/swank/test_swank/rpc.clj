(ns swank.test-swank.rpc
  (:import [java.io StringReader StringWriter])
  (:require [swank.rpc :as rpc])
  (:use clojure.test))

(defn- make-swank-reader
  ([form]
     (let [buf (StringWriter.)]
       (rpc/encode-message buf form)
       (StringReader. (.toString buf)))))

(defn- round-trip
  ([form]
     (is (= form (rpc/decode-message (make-swank-reader form))))))

(deftest encode-decode
  (round-trip "foo")
  (round-trip 42)
  (round-trip 'foo)
  (round-trip nil)
  (round-trip true)
  (round-trip '(foo))
  (round-trip :foo)
  (round-trip '(:emacs-rex (swank:connection-info) "COMMON-LISP-USER" true 1)))
