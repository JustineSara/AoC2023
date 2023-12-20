(ns t.d21
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "")

(defn parse-input
  [input]
  (cljstr/split-lines input)
)


(defn d21p1
  [input]
  (let [x (parse-input input)
        ]
     x
  ))

(defn d21p2
  [input]
  (let [x (parse-input input)
        ]
    x
  ))

(defn -main
  [& args] 
  (println "day21")
  (println sample)
  (newline)
  
  (println "part1")
  (prn (d21p1 sample))
;;  (prn (d21p1 (slurp "input/day21.txt")))
  
;;  (newline)
;;  (println "part2")
;;  (prn (d21p2 sample))
;;  (prn (d21p2 (slurp "input/day21.txt")))
  )
