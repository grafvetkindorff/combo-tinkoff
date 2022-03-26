(ns combo-tinkoff
  (:require [clojure.math.combinatorics :as combo]))

(defn make-seq [type from m]
      (map #(assoc {} :combination %1 :sum (reduce + %))
           (type from m)))

(defn make-combinations [prices m]
  (map #(assoc {} :combination %1 :sum (reduce + %))
       (combo/combinations prices m)))

(defn filter-combinations [prices m threshold]
  (let [cmbs (filter #(>= threshold (:sum %))
                     (make-combinations prices m))]
    (if (empty? cmbs) {} (apply max-key :sum cmbs))))

(defn get-the-best [prices threshold]
  (let [cnt-of-combs (range 1 (inc (count prices)))
        combinations (filter not-empty
                             (map #(filter-combinations prices % threshold) cnt-of-combs))]
    (apply max-key :sum combinations)))

(defn -main [& opts]
  (let [prices (:prices opts)
        threshold (:threshold opts)]
    (println opts)
    (println (get-the-best prices threshold))))
