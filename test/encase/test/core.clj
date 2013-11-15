(ns encase.test.core
  (:use [encase.core] :reload)
  (:use [clojure.test]))

(deftest test-clear-commands
  (with-command-stack
   (is (= {} (clear-commands!)))))
