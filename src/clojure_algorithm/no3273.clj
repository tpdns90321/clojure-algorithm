(ns clojure-algorithm.no3273
  (:require [clojure.string :as strings])
  (:gen-class))

(defn two-pointer [arr target]
  (let [arr (sort arr)
        extract (memoize #(list (first %) (last %)))
        select (memoize #(apply + (extract %)))
        predict (comp #(if (>= (select %) target) (butlast %) %)
                      #(if (<= (select %) target) (rest %) %))]
    (filter
      #(= target (select %))
      (take-while #(<= 2 (count %)) (iterate predict arr)))))

(defn -main [& args]
  (let [_ (read-line)
        arr (map #(Integer/parseInt %) (strings/split (read-line) #"\s+"))
        target (Integer/parseInt (read-line))]
    (println (count (two-pointer arr target)))))
