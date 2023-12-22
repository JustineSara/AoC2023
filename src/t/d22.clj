(ns t.d22
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "")

(defn parse-input
  [input]
  (cljstr/split-lines input)
)


(defn d22p1
  [input]
  (let [x (parse-input input)
        ]
     x
  ))

(defn d22p2
  [input]
  (let [x (parse-input input)
        ]
    x
  ))

(defn -main
  [& args] 
  (println "day22")
  (println sample)
  (newline)
  
  (println "part1")
  (prn (d22p1 sample))
;;  (prn (d22p1 (slurp "input/day22.txt")))
  
;;  (newline)
;;  (println "part2")
;;  (prn (d22p2 sample))
;;  (prn (d22p2 (slurp "input/day22.txt")))
  )
