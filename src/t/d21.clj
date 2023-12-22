(ns t.d21
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")

(defn parse-input
  [input]
  (->>
   (cljstr/split-lines input)
   (map-indexed (fn [y l] (map-indexed (fn [x c] [x y c]) l) ))
   (apply concat)
   (filter (fn [[_ _ c]] (not= c \#)))
   ((fn [all]
      [(->>
        all
        (filter (fn [[_ _ c]] (= c \S)))
        first
        ((fn [[x y _]] [x y]))
        )
       (->>
        all
        (map (fn [[x y _]] [x y]))
        set)]))
   )
)


(defn one-step
  [gardens pos-s]
  (->>
   (for [[x y] pos-s]
     (->> [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
          (filter (fn [[x y]] (contains? gardens [x y])))
          set))
   (apply cljset/union))
)


(defn walk
  [pos-s gardens Nsteps]
  (loop [n Nsteps
         p-s pos-s]
    (println "  " n " : " p-s)
    (if (zero? n)
      (count p-s)
      (->>
       p-s
       (one-step gardens)
       (recur (dec n)))
      )
    )
  )

(defn walk2
  [pos-s gardens Nsteps]
  (loop [n Nsteps
         p-s pos-s]
    (println "  " n " : " p-s)
    (if (zero? n)
      (count p-s)
      (->>
       p-s
       (one-step gardens)
       (recur (dec n))))))


(defn d21p1
  [input Nsteps]
  (let [[pos0 gardens] (parse-input input)
        ]
    (println "start at " pos0)
    ;; (one-step [pos0 pos0] gardens)
    (walk [pos0] gardens Nsteps)
  ))

(defn d21p2
  [input]
  (let [[pos0 gardens] (parse-input input)
        ]
    pos0
  ))

(defn -main
  [& args] 
  (println "day21")
  (println sample)
  (newline)
  
  (println "part1")
  ;; (prn (d21p1 sample 6))
;;  (prn (d21p1 (slurp "input/day21.txt") 64))
  
;;  (newline)
;;  (println "part2")
 (prn (d21p2 sample) 6)
;;  (prn (d21p2 (slurp "input/day21.txt")))
  )
