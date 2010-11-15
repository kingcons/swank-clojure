(ns swank.test-swank.rpc
  (:import [java.io StringReader StringWriter])
  (:require [swank.rpc :as rpc])
  (:use clojure.test))

(defn- swank-encode
  ([form]
     (let [buf (StringWriter.)]
       (rpc/encode-message buf form)
       (.toString buf))))

(defn- swank-decode
  ([swank-packet]
     (rpc/decode-message (StringReader. swank-packet))))

(defn- round-trip
  ([form]
     (is (= form (swank-decode (swank-encode form))))))

(deftest encode-decode
  (round-trip "foo")
  (round-trip 42)
  (round-trip 'foo)
  (round-trip nil)
  (round-trip true)
  (round-trip '(foo))
  (round-trip :foo)
  (round-trip '(:emacs-rex (swank:connection-info) "COMMON-LISP-USER" true 1)))

(deftest namespaces
  (is (= "swank:foo" (subs (swank-encode 'swank/foo) 6))))

(deftest read-cursor-marker
  (is (= `("+" ~(symbol "swank::%cursor-marker%"))
         (swank-decode "00001c(\"+\" swank::%cursor-marker%)"))))
