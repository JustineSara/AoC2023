(ns t.day17
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
")

(defn parse-input
  [input]
  (into {}
   (apply concat
  (map-indexed (fn [y l] (map-indexed (fn [x n] [[x y] (parse-long (str n))]) l)) (cljstr/split-lines input))
  )))

(defn fsuitesum [l a] (conj l (+ a (or (last l) 0))))

(defn next6paths
  [[heat [x y] d] heatmap]
;;   path is [heat=length-of-path position direction]
  (cond
    (= d ">") 
    (->>
     (map (fn [i] (get heatmap [(+ x i) y])) (range 1 4))
     (keep identity)
     (reduce fsuitesum [])
     (mapcat (fn [newpos h] [[(+ heat h ) newpos "^"] [(+ heat h) newpos "v"]] ) (map (fn [i] [(+ x i) y]) (range 1 4)) ))
    (= d "<") 
    (->>
     (map (fn [i] (get heatmap [(- x i) y])) (range 1 4))
     (keep identity)
     (reduce fsuitesum [])
     (mapcat (fn [newpos h] [[(+ heat h) newpos "^"] [(+ heat h) newpos "v"]]) (map (fn [i] [(- x i) y]) (range 1 4)) ))
    (= d "^") 
    (->>
     (map (fn [i] (get heatmap [x (- y i)])) (range 1 4))
     (keep identity)
     (reduce fsuitesum [])
     (mapcat (fn [newpos h] [[(+ heat h) newpos "<"] [(+ heat h) newpos ">"]]) (map (fn [i] [x (- y i)]) (range 1 4)) ))
    (= d "v") 
    (->>
     (map (fn [i] (get heatmap [x (+ y i)])) (range 1 4))
     (keep identity)
     (reduce fsuitesum [])
     (mapcat (fn [newpos h] [[(+ heat h) newpos "<"] [(+ heat h) newpos ">"]]) (map (fn [i] [x (+ y i)]) (range 1 4)) ))
  )
  )

(defn steps
  [list-paths heatmap final-pos]
  (loop [lp list-paths c 0]
    (let [[path & lp] (sort-by first lp)
        ;;   _ (println " " path)
          ]
      (when (zero? (mod c 100)) (println "  " path " - " (count lp)) )
      (if (= (second path) final-pos)
        (first path)
        (recur (concat (next6paths path heatmap) lp) (inc c))
        )
      )
  ))

(defn d17
  [input]
  (let [heatmap (parse-input input)
        maxX (apply max (map first (keys heatmap)))
        maxY (apply max (map second (keys heatmap)))
        final-pos [maxX maxY]]
    (println "part1")
    (println "  map of heat is" (inc maxX) "x" (inc maxY))
    (steps [[0 [0 0] "v"][0 [0 0] ">"]] heatmap final-pos)
    )
  )

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

  )