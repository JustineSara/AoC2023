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

(defn manhatan_dist 
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))


(defn compare-paths
  [[h1 p1 _] [h2 p2 _] final-pos]
  (if (= p1 p2)
    (< h1 h2)
    (< (manhatan_dist p1 final-pos) (manhatan_dist p2 final-pos)))
  )

(defn step-equal-paths
  [list-eq-paths heatmap seen]
  (let [newp (->>
              list-eq-paths
              (mapcat #(next6paths % heatmap))
              (sort-by first))]
    (loop [paths newp
           s seen
           res []]
      (if (zero? (count paths))
        [res s]
        (let [[p & paths] paths
              [h [x y] d] p]
          (if (contains? seen [x y d])
            (recur paths s res)
            (recur paths (conj s [x y d]) (conj res p) )))
        )
      )
  )
  )

(defn steps
  [list-paths heatmap final-pos]
  ;; path = [heat [x y] d]
  ;; seen = [ [x y d] ]
  (loop [lp list-paths 
         c 0 
         seen (set (map (fn [[h [x y] d]] [x y d]) list-paths))] 
        ;;  seen (into {} (map (fn [[h [x y] d]] [[x y d] h]) list-paths))] 
         
    ;; best-path ##Inf]
    (let [lp (sort-by first lp)
          path (first lp)
        ;;   _ (println " " path)
          ]
      (when (zero? (mod c 100)) (println "  " path " - " (count lp)) )
      (if (= (second path) final-pos)
        (first path)
        (let [ 
              [l-equal-paths lp] (split-with #(= (first %) (first path)) lp)
              _ (prn l-equal-paths)
              [newpaths newseen] (step-equal-paths l-equal-paths heatmap seen)
            ;;   newpath (->
            ;;             path
            ;;             (next6paths heatmap)
            ;;             (->> (filter (fn [[_ [x y] d]] (not (contains? seen [x y d]))) )))
              ]
        ;;  (recur (concat newpath lp) (inc c) (apply conj seen (map (fn [[_ [x y] d]] [x y d]) newpath)))
        ;;  (recur (concat newpath lp) (inc c) (reduce #(assoc %1 (first %2) (second %2)) seen (map (fn [[h [x y] d]] [[x y d] h]) newpath)))
          (recur (concat newpaths lp) (inc c) newseen)
          )
      )
  )))

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