(ns t.d23
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#")

(defn parse-input
  [input]
  (cljstr/split-lines input)
)


(defn d23p1
  [input]
  (let [x (parse-input input)
        ]
     x
  ))

(defn d23p2
  [input]
  (let [x (parse-input input)
        ]
    x
  ))

(defn -main
  [& args] 
  (println "day23")
  (println sample)
  (newline)
  
  (println "part1")
  (prn (d23p1 sample))
;;  (prn (d23p1 (slurp "input/day23.txt")))
  
;;  (newline)
;;  (println "part2")
;;  (prn (d23p2 sample))
;;  (prn (d23p2 (slurp "input/day23.txt")))
  )
