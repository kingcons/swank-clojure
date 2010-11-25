;;; This code has been placed in the Public Domain.  All warranties are disclaimed.
(ns #^{:doc "Pass remote calls and responses between lisp systems using the swank-rpc protocol."
       :author "Terje Norderhaug <terje@in-progress.com>"}
  swank.rpc
  (:use (swank util)
        (swank.util io))
  (:import (java.io Writer Reader PushbackReader StringReader)
           (clojure.lang Symbol)))

;; ERROR HANDLING

(def swank-protocol-error (Exception. "Swank protocol error."))

;; LOGGING

(def log-events false)

(def log-output nil)

(defn log-event [format-string & args]
  (when log-events
    (.write (or log-output *out*) (apply format format-string args))
    (.flush (or log-output *out*))
    nil))

;; INPUT

(declare read-form read-sequence-form)

(defn- read-sequence-form
  ([#^Reader rdr closing-char]
     (let [closing-char (int closing-char)]
       (loop [forms []]
         (let [next-char (.read rdr)]
           (condp = next-char
               -1 (throw (Exception. "Incomplete sequence."))
               closing-char forms
               (do
                 (when (not= next-char 32) (.unread rdr next-char))
                 (recur (conj forms (read-form rdr))))))))))

(defn- read-form
   "Read a form that conforms to the swank rpc protocol"
  ([#^Reader rdr]
    (let [c (.read rdr)]
      (condp = (char c)
          \" (let [sb (StringBuilder.)]
               (loop []
                (let [c (.read rdr)]
                 (if (= c -1)
                   (throw (java.io.EOFException. "Incomplete reading of quoted string."))
                   (condp = (char c)
                     \" (str sb)
                     \\ (do (.append sb (char (.read rdr)))
                            (recur))
                    (do (.append sb (char c))
                        (recur)))))))
          \( (list* (read-sequence-form rdr \)))
          \[ (vec (read-sequence-form rdr \]))
          \' (list 'quote (read-form rdr))
          (let [sb (StringBuilder.)]
            (loop [c c]
              (if (not= c -1)
                (case (char c)
                      \\ (do (.append sb (char (.read rdr)))
                             (recur (.read rdr)))
                      (\space \) \]) (.unread rdr c)
                      (do (.append sb (char c))
                          (recur (.read rdr))))))
            (let [str (str sb)]
              (cond
                (. Character isDigit c) (Integer/parseInt str)
                (= "nil" str) nil
                (= "t" str) true
                (= \: (char c)) (keyword (subs str 1))
                :else (if-let [[_ symbol-ns symbol-name]
                               (re-matches #"^([^:]+):([^:]+)$" str)]
                        (symbol symbol-ns symbol-name)
                        (symbol str)))))))))

(defn- read-packet
  ([#^Reader reader]
     (let [len (Integer/parseInt (read-chars reader 6 swank-protocol-error) 16)]
       (read-chars reader len swank-protocol-error))))

(defn decode-message
   "Read an rpc message encoded using the swank rpc protocol."
  ([#^Reader rdr]
    (let [packet (read-packet rdr)]
       (log-event "READ: %s\n" packet)
       (try
         (with-open [rdr (PushbackReader. (StringReader. packet))]
           (read-form rdr))
         (catch Exception e
           (list :reader-error packet e))))))

; (with-open [rdr (StringReader. "00001f(swank:a 123 (%b% (t nil) \"c\"))")] (decode-message rdr))


;; OUTPUT

(defmulti print-object (fn [x writer] (type x)))

(defmethod print-object :default [o, #^Writer w]
  (print-method o w))

(defmethod print-object Symbol [o, #^Writer w]
  (let [symbol-ns (namespace o)
        symbol-name (name o)]
    (.write w (if symbol-ns
                (format "%s:%s" symbol-ns symbol-name)
                symbol-name))))

(defmethod print-object Boolean [o, #^Writer w]
  (.write w (if o "t" "nil")))

(defmethod print-object String [#^String s, #^Writer w]
  (let [char-escape-string {\" "\\\""
                            \\  "\\\\"}]
    (do (.append w \")
      (dotimes [n (count s)]
        (let [c (.charAt s n)
              e (char-escape-string c)]
          (if e (.write w e) (.append w c))))
      (.append w \"))
  nil))

(defmethod print-object clojure.lang.ISeq [o, #^Writer w]
  (.write w "(")
  (print-object (first o) w)
  (doseq [item (rest o)]
    (.write w " ")
    (print-object item w))
  (.write w ")"))

(defn- write-form
  ([#^Writer writer message]
    (print-object message writer)))

(defn- write-packet
  ([#^Writer writer str]
   (let [len (.length str)]
    (doto writer
          (.write (format "%06x" len))
          (.write str)
          (.flush)))))

(defn encode-message
  "Write an rpc message encoded using the swank rpc protocol."
  ([#^Writer writer message]
     (let [str (with-out-str
                  (write-form *out* message)) ]
       (log-event "WRITE: %s\n" str)
       (write-packet writer str))))

; (with-out-str (encode-message *out* "hello"))
; (with-out-str (encode-message *out* '(a 123 (swank:b (true false) "c"))))


;; DISPATCH

(defonce rpc-fn-map {})

(defn register-dispatch
  ([name fn]
    (register-dispatch name fn #'rpc-fn-map))
  ([name fn fn-map]
    (alter-var-root fn-map assoc name fn)))

(defn dispatch-message
  ([message fn-map]
    (let [operation (first message)
          operands (rest message)
          fn (fn-map operation)]
        (assert fn)
        (apply fn operands)))
  ([message]
   (dispatch-message message rpc-fn-map)))
