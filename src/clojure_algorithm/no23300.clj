(ns clojure-algorithm.no23300
  (:require [clojure.string :as s]))

(def init-browser {
  :current nil
  :backward nil
  :forward nil
})

(defn go-direction [direction browser]
  (let [reverse-direction (case direction :backward :forward :forward :backward)
        {
          current :current
          target direction
          reverse-target reverse-direction
        } browser]
    (if-not (empty? target)
      (assoc
        browser
        :current (peek target)
        direction (pop target)
        reverse-direction (conj reverse-target current))
      browser)))

(defn access [target browser]
  (let [{
          current :current
          backward :backward
        } browser]
    (assoc browser
      :current target
      :backward (when-not (nil? current) (conj backward current))
      :forward nil)))

(defn compress [browser]
  (let [{
          backward :backward
        } browser
        start (first backward)
        back-front (partition 2 1 backward)]
    (assoc browser
      :backward (conj (map second (filter #(apply not= %) back-front)) start))))

(defn solver! [in]
  (loop [num_of_ops (second (map #(Integer/parseInt %) (s/split (.readLine in) #" ")))
         browser init-browser]
    (if-not (= num_of_ops 0)
      (let [[ops target] (s/split (.readLine in) #" ")
            browser (condp = ops
                      "A" (access target browser)
                      "B" (go-direction :backward browser)
                      "F" (go-direction :forward browser)
                      "C" (compress browser))]
        (recur (- num_of_ops 1) browser))
      (letfn [(print-history [direction {target direction}]
                              (if-not (empty? target)
                                (println (s/join " " target))
                                (println -1)))]
        (println (:current browser))
        (print-history :backward browser)
        (print-history :forward browser)))))
