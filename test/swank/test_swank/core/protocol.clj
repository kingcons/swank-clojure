(ns swank.test-swank.core.protocol
  (:import (java.io StringReader
                    StringWriter))
  (:use clojure.test
        swank.core.protocol))

(defn- read-swank-message-str
  ([message]
     (with-open [reader (StringReader. message)]
       (read-swank-message reader))))

(defn- write-swank-message-str
  ([form]
     (with-open [writer (StringWriter.)]
       (write-swank-message writer form)
       (.toString writer))))

(deftest round-trip-messages
  (are [form form-str]
       (let [prefixed-form-str (format "%06x%s"
                                       (count form-str)
                                       form-str)]
         (and (= (read-swank-message-str prefixed-form-str) form)
              (= (write-swank-message-str form) prefixed-form-str)))

       42 "42"
       "foo" "\"foo\""
       nil "nil"
       true "t"
       'foo "foo"
       'foo/bar "foo:bar"
       :foo ":foo"
       '(a b c) "(a b c)"
       '[a b c] "[a b c]"
       '(:keyword "string") "(:keyword \"string\")"
       '(nested (list [vector])) "(nested (list [vector]))"
       [] "[]"

       '(:emacs-rex (swank/connection-info) "COMMON-LISP-USER" true 1)
       "(:emacs-rex (swank:connection-info) \"COMMON-LISP-USER\" t 1)"
       ))

(deftest read-cursor-marker
  (is (= (read-swank-message-str "00001c(\"+\" swank::%cursor-marker%)")
         `("+" ~(symbol "swank::%cursor-marker%")))))
