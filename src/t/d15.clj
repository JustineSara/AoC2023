(ns t.d15
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))


(def sample "")

(defn d15
  [input]
  (cljstr/split-lines input)
  )


(defn -main
  [& args]
  (println "day15")
  (println sample)
  (newline)
  (prn (d15 sample))
  #_(println (d15 (slurp "input/day15.txt")))
  (newline)
  )