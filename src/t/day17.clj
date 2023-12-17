(ns t.d17
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "")


(defn d17
  [input]
  (cljstr/split-lines input))

(defn -main
  [& args]
  (println "day17")
  (println sample)
  (prn (d17 sample))
  (newline)
;;   (println (d17 (slurp "input/day17.txt")))
;;   (newline)
;;   (prn (d17p2 sample))
;;   (newline)
;;   (println (d17p2 (slurp "input/day17.txt")))
;;   )