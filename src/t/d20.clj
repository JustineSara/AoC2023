(ns t.d20
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(def sample2 "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")


(defn parse-input
  [input]
  (cljstr/split-lines input)
)


(defn d20p1
  [input]
  (let [x (parse-input input)
        ]
     x
  ))

(defn d20p2
  [input]
  (let [x (parse-input input)
        ]
    x
  ))

(defn -main
  [& args] 
  (println "day<DAYNUM")
  (println sample)
  (newline)
  
  (println "part1")
  (prn (d20p1 sample))
;;  (prn (d20p1 (slurp "input/day20.txt")))
  
;;  (newline)
;;  (println "part2")
;;  (prn (d20p2 sample))
;;  (prn (d20p2 (slurp "input/day20.txt")))
  )
