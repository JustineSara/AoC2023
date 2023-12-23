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
  (->> (cljstr/split-lines input)
       (map-indexed (fn [y l] (map-indexed (fn [x c] [[x y] c]) l)))
       (apply concat)
       (filter (fn [[_ c]] (not= \# c)) )
       (into {})
       ))

(defn one-step
  [[x y :as pos] forest]
  (let [type (get forest pos)]
    (->> 
      (cond 
        (= type \.) [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
        (= type \>) [[(inc x) y]]
        (= type \<) [[(dec x) y]]
        (= type \^) [[x (dec y)]]
        (= type \v) [[x (inc y)]] )
      (filter (fn [p] (contains? forest p))))
    )
  )

(def one-step-mm (memoize one-step))

(defn find-start-pos
  [forest]
  (->> forest
       (filter (fn [[[x y] _]] (= y 0) ))
       (filter (fn [[_ c]] (= c \.)))
       (first)
       (first)
       )
  )

(defn find-end-pos
  [forest]
  (->> forest
       (filter (fn [[[x y] _]] (= y (apply max (map (fn [[[_ y] _]] y) forest)) )))
       (filter (fn [[_ c]] (= c \.)))
       (first)
       (first)))


(defn one-step-all-paths
  [all-paths forest]
  ;; one path is {:visited :pos}
  (loop [paths-tbd all-paths
         paths-done []
         ]
    (if (empty? paths-tbd)
      paths-done
      (let [[path & paths-tbd] paths-tbd
            pos (:pos path)
            new-pos (->> (one-step-mm pos forest)
                         (filter (fn [p] (not (contains? (:visited path) p)))))
            new-paths (map (fn [p] {:pos p :visited (conj (:visited path) p)} ) new-pos)
            ]
        (if (empty? new-pos)
          (recur paths-tbd paths-done)
          (recur paths-tbd (concat paths-done new-paths))
          )
        )
    )
   )
  )

(defn search-longest-path
  [forest]
  (let [start (find-start-pos forest)
        end   (find-end-pos forest)]
    (loop [paths [{:pos start :visited #{start}}]
           best-path 0
           iter 0]
      ;; (println " " (count paths))
      ;; (println " " paths)
      (if (empty? paths)
      ;; (if (= iter 3)
        best-path
        (let [next-paths (one-step-all-paths paths forest)
              ;; _ (println " " next-paths)
              ended-paths-length (->> next-paths
                                      (filter (fn [{p :pos}] (= p end)))
                                      (map (fn [{v :visited}] (count v))))
              next-paths (filter (fn [{p :pos}] (not= p end)) next-paths)
              ]
          (recur next-paths (apply max best-path ended-paths-length) (inc iter))
        )
      )
   ))
  )


(defn d23p1
  [input]
  (let [forest (parse-input input)
        ]
    #_(do
      (println "~~ test one-step ~~")
      (println "  from [1 0]")
      (println "  --> " (one-step [1 0] forest))
      (println "from [1 1]")
      (println "  --> " (one-step [1 1] forest))
      (println "from [3 4]")
      (println "  --> " (one-step [3 4] forest))
      (println "from [10 3]")
      (println "  --> " (one-step [10 3] forest)))
    #_(do
      (println "~~ test find-start-pos ~~")
      (println "  " (find-start-pos forest)))
    #_(do
        (println "~~ test one-step-all-paths ~~")
        (println "from [1 0] (start):")
        (println "  --> " (one-step-all-paths [{:pos [1 0] :visited #{[1 0]}}] forest))
        (println "then")
        (println "  --> " (one-step-all-paths [{:pos [1 1], :visited #{[1 0] [1 1]}}] forest)))
    
    (dec (search-longest-path forest))
    ;; dec because we want the number of steps, not the number of tiles visited

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
  (prn (d23p1 (slurp "input/day23.txt")))
  
;;  (newline)
;;  (println "part2")
;;  (prn (d23p2 sample))
;;  (prn (d23p2 (slurp "input/day23.txt")))
  )
