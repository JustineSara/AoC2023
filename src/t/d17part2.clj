(ns t.d17part2
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

(def sample2 "111111111111
999999999991
999999999991
999999999991
999999999991")

(defn parse-input
  [input]
  (into {}
        (apply concat
               (map-indexed (fn [y l] (map-indexed (fn [x n] [[x y] (parse-long (str n))]) l)) (cljstr/split-lines input)))))


;; DIRECTIONS
;; dir : {">" n-step-done}

(defn next-one-d
  [x y d nd]
  (cond
    (< nd 3)
    (cond
      (= d ">") [[(inc x) y] {">" (inc nd)}]
      (= d "<") [[(dec x) y] {"<" (inc nd)}]
      (= d "^") [[x (dec y)] {"^" (inc nd)}]
      (= d "v") [[x (inc y)] {"v" (inc nd)}])
    (< nd 9)
    (cond
      (= d ">") [[(inc x) y] {">" (inc nd) "^" 0 "v" 0}]
      (= d "<") [[(dec x) y] {"<" (inc nd) "^" 0 "v" 0}]
      (= d "^") [[x (dec y)] {"^" (inc nd) ">" 0 "<" 0}]
      (= d "v") [[x (inc y)] {"v" (inc nd) ">" 0 "<" 0}])
    (= nd 9)
    (cond
      (= d ">") [[(inc x) y] {"^" 0 "v" 0}]
      (= d "<") [[(dec x) y] {"^" 0 "v" 0}]
      (= d "^") [[x (dec y)] {">" 0 "<" 0}]
      (= d "v") [[x (inc y)] {">" 0 "<" 0}])
    ))

(defn next-p
  [[h [x y] dir] heatmap]
  (->> dir
       (map (fn [[d nd]] (next-one-d x y d nd)))
       (map (fn [[p d]] (when (contains? heatmap p) [(+ h (get heatmap p)) p d])))
       (keep identity)))


;; SEEN - a set
;; #{ [x y d nd] , ... }
;; nd := number in this direction

(defn check-seen
  [[h [x y] dirs] seen]
  (->>
   dirs
   (filter (fn [[d vd]] (not (contains? seen [x y d vd]))))
   ((fn [newdirs] [[h [x y] (into {} newdirs)]
                   (apply conj seen newdirs)]))
   ))

(defn find-one-path-p2
  [heatmap maxX maxY]
  (let [m (quot maxX 4)
        s (+ (mod maxX 4) 4)
        [fsections [lsx lsy]] ((juxt drop-last last) (map (fn [n] [(* 4 n) (* 4 n)]) (range m)))]
    (->>
     (concat
       (mapcat (fn [[x y]] (concat (map (fn [xi] [(+ x xi) y]) (range (inc 4)) ) (map (fn [yi] [(+ x 4) (+ yi y)]) (range 1 4) ) )) fsections)
       (map (fn [xi] [(+ lsx xi) lsy]) (range (inc s)))
       (map (fn [yi] [(+ lsx s) (+ yi lsy)]) (range 1 (inc s))))
     (map (fn [pos] (get heatmap pos)))
     (apply +)
     )
    )
  )

(defn find-path
  [all-paths seen heatmap final-pos c one-path-value]
  (when (zero? (mod c 10000)) (println "   " c " : " (count all-paths) "- min & max" (apply min (map first all-paths)) (apply max (map first all-paths))))
  (let [all-paths (sort-by first all-paths)
        path (first all-paths)
        all-paths (rest all-paths)]
    ;; (println "path :" path)
    ;; (println "rest :" all-paths)
    (cond
      (= (second path) final-pos)
      (if (>= (apply max (vals (last path))) 4)
        (first path)
        (recur all-paths seen heatmap final-pos (inc c) one-path-value))
      (and (zero? (count all-paths)) (not= c 0))
      (println "/!\\No path to explore but no results/!\\")
      :else
      (let [pot-paths (next-p path heatmap)
            pot-paths (filter (fn [[h _ _]] (<= h one-path-value)) pot-paths)
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
        (recur (concat all-paths new-all-paths) new-seen heatmap final-pos (inc c) one-path-value)))))


(defn manhatan_dist
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2)))
  )


(defn next-dir
  [d nd]
  (cond
    (< nd 3)
    [[d (inc nd)]]
    (< nd 9)
    (cond
      (= d ">") [["v" 0] [">" (inc nd)] ["^" 0]]
      (= d "<") [["v" 0] ["<" (inc nd)] ["^" 0]]
      (= d "^") [[">" 0] ["^" (inc nd)] ["<" 0]]
      (= d "v") [[">" 0] ["v" (inc nd)] ["<" 0]])
    (= nd 9)
    (cond
      (= d ">") [["v" 0] ["^" 0]]
      (= d "<") [["v" 0] ["^" 0]]
      (= d "^") [[">" 0] ["<" 0]]
      (= d "v") [[">" 0] ["<" 0]])
    ))

(defn next-path
  [[h [x y] dir nd] heatmap]
  (let [dirs (next-dir dir nd)
        n-pos (case dir ">" [(inc x) y] "<" [(dec x) y] "^" [x (dec y)] "v" [x (inc y)])
        n-h (get heatmap n-pos)]
    (when n-h
      (map (fn [[n-dir n-nd]] [(+ h n-h) n-pos n-dir n-nd]) dirs)
      )
))

(defn find-path-all-cases
  [all-paths seen cur-min final-pos heatmap c]
  ;; one path is : [heat pos d nd]
  ;; seen is a map: [ [x y d nd] h ]
  ;; cur-min : a value of the current minimum path
  (when (zero? (mod c 1000000)) (println "   " c " : " (count all-paths) 
                                       "- min & max" (apply min (map first all-paths)) (apply max (map first all-paths))
                                       "- current min" cur-min))
  (if
    (empty? all-paths)
    cur-min
    (let [
        ;;   all-paths (sort-by (fn [[_ pos _ _]] (manhatan_dist pos final-pos)) all-paths)
          [path & all-paths] all-paths
          ]
    ;;   (prn path)
    ;;   (prn (count all-paths))
      (if (= (second path) final-pos)
        (do
          (println "      path in final position :" path)
         (if (< (last path) 4)
          (recur all-paths seen cur-min final-pos heatmap (inc c))
          (recur all-paths seen (min cur-min (first path)) final-pos heatmap (inc c))
          ))
        (let [nxt-paths (next-path path heatmap)
            ;;   _ (println "new paths before filter:" nxt-paths)
              nxt-paths (filter (fn [[h _ _ _]] (< h cur-min)) nxt-paths)
              nxt-paths (filter (fn [[h [x y] d nd]] (or (not (contains? seen [x y d nd])) (< h (get seen [x y d nd])))) nxt-paths)
            ;;   _ (println "new paths after filter:" nxt-paths)
              new-seen (if (empty? nxt-paths) seen (apply assoc seen (mapcat (fn [[h [x y] d nd]] [[x y d nd] h]) nxt-paths)))
            ;;   _ (println seen)
            ;;   _ (println new-seen)
              ]
          (recur (concat nxt-paths all-paths) new-seen cur-min final-pos heatmap (inc c))
          )
        )
    )
  )
)

(defn d17
  [input]
  (let [heatmap (parse-input input)
        maxX (apply max (map first (keys heatmap)))
        maxY (apply max (map second (keys heatmap)))
        final-pos [maxX maxY]
        one-path-value (find-one-path-p2 heatmap maxX maxY)]
    (println "part2")
    (println "  map of heat is" (inc maxX) "x" (inc maxY))
    (println "  one path has a value of " one-path-value)
    ;; (find-path [[0 [0 0] {">" 0 "v" 0}]] {[0 0 ">"] 0 [0 0 "v"] 0} heatmap final-pos 0 one-path-value)
    (find-path-all-cases [[0 [0 0] "v" 0] [0 [0 0] ">" 0]] {[0 0 ">" 0] 0 , [0 0 "v" 0] 0} one-path-value final-pos heatmap 0)

    
    ;; (next-p [0 [0 0] {">" 0, "v" 0}] heatmap)
    ))

(defn -main
  [& args]
  (println "day17")
;;   (println sample2)
;;   (prn (d17 sample2))
;;   (newline)
  (println sample)
  (prn (d17 sample))
  (newline)
  (println (d17 (slurp "input/day17.txt")))

  )

;; 1587 is too high