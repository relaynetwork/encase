(ns encase.core
  (:require
   [clojure.contrib.json :as json]
   [clojure.contrib.seq-utils :as seq]
   [clj-etl-utils.log      :as log])
  (:use
   [clj-etl-utils.lang-utils :only [raise]]))


(defonce *commands* (atom {}))

(defmacro assert! [msg & expr]
  `(let [b# '~expr
         r# (do ~@expr)]
     (if-not r#
       (raise ~msg)
       true)))

(defn make-command [n ver f sig]
  {:name (name n)
   :version ver
   :signature sig
   :function f})

(defn register-command! [n ver f sig]
  (swap! *commands* assoc-in [(name n) ver] (make-command (name n) ver f sig)))

(defn lookup-command
  ([n ver]
     (get-in @*commands* [(name n) ver]))
  ([cmd-map]
     (let [cmd (get-in @*commands* [(name  (:name cmd-map))
                                    (:version cmd-map)])]
       (assert! (format "Error: command(%s) not found." cmd-map)
                cmd)
       cmd)))

;; TODO: support var args?
;; TODO: support variable type? (a set? maybe just a predicate function?)
;; TODO: support (or type= nil?)
(defn validate-command-instance! [cmd-map]
  (if-not (lookup-command cmd-map)
    (raise "Error: command (or version) not registered in: '%s'" cmd-map))
  (doseq [k [:name :version :args]]
    (printf "validing has key: %s\n" k)
    (assert! (format "Missing key: %s" k)
             (contains? cmd-map k)))
  (let [sig  (:signature (lookup-command cmd-map))
        args (:args cmd-map)]
    (doseq [[idx arg-spec] (seq/indexed sig)]
      (printf "validaing: %s isa %s\n"
              (nth args idx)
              (:type arg-spec))
      (if-not (isa? (class (nth args idx))
                    (:type arg-spec))
        (raise "Error: invalid command, type of %s is %s expected %s in %s"
               (:name arg-spec)
               (class (nth args idx))
               (:type arg-spec)
               cmd-map))))
  cmd-map)


(defn make-instance* [n ver args]
  (let [cmd  (lookup-command n ver)
        body {:name (name n)
              :version ver
              :args args}]
    body))

(defn make-instance [n ver & args]
  (make-instance* n ver args))

(defn make-instance*! [n ver args]
  (validate-command-instance! (make-instance* n ver args)))

(defn make-instance! [n ver & args]
  (make-instance*! n ver args))

(defn make-instance-json [n ver & args]
  (json/json-str (make-instance* n ver args)))

(defn make-instance-json! [n ver & args]
  (printf "make-instance-json! %s %s %s\n" n ver args)
  (json/json-str (make-instance*! n ver args)))

(defn json-to-command [s]
  (json/read-json s))


(defn invoke [cmd-map]
  (let [spec (lookup-command cmd-map)
        f    (:function spec)]
    (log/infof "cmd: invoke: cmd-map=%s spec=%s f=%s\n" cmd-map spec f)
    (apply f (:args cmd-map))))


(def *command-stack* nil)
(def defer-command!  nil)
(def commands        nil)

(defn with-command-stack* [f]
  (binding [*command-stack* (atom[])
            defer-command!  (fn [command]
                              (swap! *command-stack* conj command))
            commands        (fn [] @*command-stack*)]
    (f)))

(defmacro with-command-stack [& body]
  `(with-command-stack*
     (fn []
       ~@body)))