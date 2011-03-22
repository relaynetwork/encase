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

(defn make-command [name ver f sig]
  {:name name
   :version ver
   :signature sig
   :function f})

(defn register-command! [name ver f sig]
  (swap! *commands* assoc-in [name ver] (make-command name ver f sig)))

(defn lookup-command
  ([n ver]
     (get-in @*commands* [n ver]))
  ([cmd-map]
     (let [cmd (get-in @*commands* [(str  (:name cmd-map))
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


(defn make-instance* [name ver args]
  (let [cmd  (lookup-command name ver)
        body {:name name
              :version ver
              :args args}]
    body))

(defn make-instance [name ver & args]
  (make-instance* name ver args))

(defn make-instance*! [name ver args]
  (validate-command-instance! (make-instance* name ver args)))

(defn make-instance! [name ver & args]
  (make-instance*! name ver args))

(defn make-instance-json [name ver & args]
  (json/json-str (make-instance* name ver args)))

(defn make-instance-json! [name ver & args]
  (printf "make-instance-json! %s %s %s\n" name ver args)
  (json/json-str (make-instance*! name ver args)))

(defn json-to-command [s]
  (json/read-json s))


(defn invoke [cmd-map]
  (let [spec (lookup-command cmd-map)
        f    (:function spec)]
    (log/infof "cmd: invoke: cmd-map=%s spec=%s f=%s\n" cmd-map spec f)
    (apply f (:args cmd-map))))

(defmacro defcommand [name version args & body]
  (let [fn-args (vec (map #(symbol (.substring (str (:name %1)) 1)) args))]
    `(do
       (defn ~name ~fn-args ~@body)
       (swap! ~'*commands* conj [~(str name) ~version ~name ~args]))))

(defn register-commands! [commands]
  (doseq [[name ver f args] commands]
    (register-command! name ver f args)))