(ns t.d17take2
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


;; DIRECTIONS
;; dir : {">" n-step-done}

(defn next-one-d
  [x y d nd]
  (when (< nd 3)
    (cond 
      (= d ">") [[(inc x) y] {">" (inc nd) "^" 0 "v" 0}]
      (= d "<") [[(dec x) y] {"<" (inc nd) "^" 0 "v" 0}]
      (= d "^") [[x (dec y)] {"^" (inc nd) ">" 0 "<" 0}]
      (= d "v") [[x (inc y)] {"v" (inc nd) ">" 0 "<" 0}]
      )
    )
  )

(defn next-p
  [[h [x y] dir] heatmap]
  (->> dir
       (map (fn [[d nd]] (next-one-d x y d nd)))
       (map (fn [[p d]] (when (contains? heatmap p) [(+ h (get heatmap p)) p d])))
       (keep identity)
       )
  )


;; SEEN
;; { [x y d] v , ... }
;; 

(defn check-seen
  [[h [x y] dirs] seen]
  (->>
   dirs
   (map (fn [[d vd]] 
          (if (contains? seen [x y d])
            (when (> (get seen [x y d]) vd)
              [d vd])
            [d vd])
          ))
   (keep identity)
   ((fn [newdirs] [[h [x y] (into {} newdirs)] 
                   (reduce #(assoc %1 [x y (first %2)] (second %2)) seen newdirs) ]))
   )
  )

(defn find-path
  [all-paths seen heatmap final-pos c]
  (when (zero? (mod c 10000)) (println "   " c " : " (count all-paths)))
  (let [all-paths (sort-by first all-paths)
        path (first all-paths)
        all-paths (rest all-paths)]
    ;; (println "path :" path)
    ;; (println "rest :" all-paths)
    (if (= (second path) final-pos)
      (first path)
      (let [pot-paths (next-p path heatmap)
            ;; _ (println "pot-paths" pot-paths)
            [new-all-paths new-seen] (loop [pp pot-paths
                                            np []
                                            s seen]
                                       (if (zero? (count pp))
                                         [np s]
                                         (let [p (first pp)
                                               pp (rest pp)
                                               [p news] (check-seen p s)]
                                           (recur pp (concat [p] np) news))))]
        (recur (concat all-paths new-all-paths) new-seen heatmap final-pos (inc c))
        
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
    (find-path [[0 [0 0] {">" 0 "v" 0}]] {[0 0 ">"] 0 [0 0 "v"] 0} heatmap final-pos 0)
    ;; (next-p [0 [0 0] {">" 0, "v" 0}] heatmap)
    
    ))

(defn -main
  [& args]
  (println "day17")
  (println sample)
  (prn (d17 sample))
  (newline)
  (println (d17 (slurp "input/day17.txt")))
;;   (newline)
;;   (prn (d17p2 sample))
;;   (newline)
;;   (println (d17p2 (slurp "input/day17.txt")))
  )