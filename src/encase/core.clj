(ns encase.core
  (:require
   [clojure.data.json        :as json]
   [clojure.tools.logging    :as log])
  (:use
   [clj-etl-utils.lang-utils :only [raise]]))


(defonce command-registry (atom {}))

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
  (swap! command-registry assoc-in [(name n) ver] (make-command (name n) ver f sig)))

(defn lookup-command
  ([n ver]
     (get-in @command-registry [(name n) ver]))
  ([cmd-map]
     (let [cmd (get-in @command-registry [(name  (:name cmd-map))
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
    (doseq [[idx arg-spec] (map-indexed vector sig)] ;; (seq/indexed sig)
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

(defonce ^{:dynamic true} command-stack nil)
(defonce ^{:dynamic true} defer-command!  nil)
(defonce ^{:dynamic true} commands        nil)

(defn with-command-stack* [f]
  (binding [command-stack   (atom[])
            defer-command!  (fn [command]
                              (swap! command-stack conj command))
            commands        (fn [] @command-stack)]
    (f)))

(defmacro with-command-stack [& body]
  `(with-command-stack*
     (fn []
       ~@body)))

(defmacro defcommand [fname version args & body]
  (let [fn-name (symbol (name fname))
        fn-args (vec (map #(symbol (name (:name %1))) args))]
    `(do
       (defn ~fn-name ~fn-args ~@body)
       (swap! ~'command-registry conj [~(str fn-name) ~version ~fn-name ~args]))))



(defn register-commands! [commands]
  (doseq [[cmd-name ver f args] commands]
    (register-command! cmd-name ver f args)))

(defn clear-commands! []
  (reset! command-stack {}))

(defn exec-commands! []
  (doseq [cmd (commands)]
    (invoke cmd)))
