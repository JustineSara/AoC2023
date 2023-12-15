(ns t.d14
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))


(def sample "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(defn parse-input
  [input]
  (let [lines (cljstr/split-lines input)]
    [ [(count (first lines)) (count lines)]
      (->> 
        (map-indexed (fn [y l] (map-indexed (fn [x c] [x y c]) l) ) lines)
        (apply concat)
        (filter (fn [[_ _ c]] (not= c \.)))
        (group-by #(nth % 2)))
     ]
  ))

(defn roll-N-one-rock
  ([[xr yr cr] allrocks] ;; the first input is the rock that might move
  [xr
    (->> 
      allrocks
      (filter (fn [[x y _]] (and (= x xr) (< y yr))) )
      (map second)
      (apply max -1)
      (inc))
    cr])
  ([a b _] (roll-N-one-rock a b))
)

(defn roll-W-one-rock
  ([[xr yr cr] allrocks] ;; the first input is the rock that might move
  [(->>
    allrocks
    (filter (fn [[x y _]] (and (= y yr) (< x xr))))
    (map first)
    (apply max -1)
    (inc))
   yr
   cr])
  ([a b _] (roll-W-one-rock a b)))

(defn roll-S-one-rock
  [[xr yr cr] allrocks maxY] ;; the first input is the rock that might move
  [xr
   (->>
    allrocks
    (filter (fn [[x y _]] (and (= x xr) (> y yr))))
    (map second)
    (apply min maxY)
    (dec))
   cr])

(defn roll-E-one-rock
  [[xr yr cr] allrocks maxX] ;; the first input is the rock that might move
  [(->>
    allrocks
    (filter (fn [[x y _]] (and (= y yr) (> x xr))))
    (map first)
    (apply min maxX)
    (dec))
   yr
   cr])

(defn roll-North
  [mrocks]
  (let [rollingstones (sort-by second (get mrocks \O))
        fixed (get mrocks \#)]
    (loop [toberolled rollingstones
           alreadyrolled []]
      (if (zero? (count toberolled))
        {\O alreadyrolled \# fixed}
        (let [stone (first toberolled)
              toberolled (rest  toberolled)
              allstones (concat fixed toberolled alreadyrolled)
              ]
          (recur toberolled (conj alreadyrolled (roll-N-one-rock stone allstones)))
          ))
      )
   )
  )

(defn roll-dir
  [mrocks maxX maxY dir]
  (let [sort-fct-1 (case dir "N" second "S" second "E" first "W" first)
        sort-fct-2 (case dir "N" < "S" > "E" > "W" <)
        maxXY (case dir "N" maxY "S" maxY "E" maxX "W" maxX)
        roll-fct (case dir "N" roll-N-one-rock "S" roll-S-one-rock "E" roll-E-one-rock "W" roll-W-one-rock)
        rollingstones (sort-by sort-fct-1 sort-fct-2 (get mrocks \O))
        fixed (get mrocks \#)]
    (loop [toberolled rollingstones
           alreadyrolled []]
      (if (zero? (count toberolled))
        {\O alreadyrolled \# fixed}
        (let [stone (first toberolled)
              toberolled (rest  toberolled)
              allstones (concat fixed toberolled alreadyrolled)]
          (recur toberolled (conj alreadyrolled (roll-fct stone allstones maxXY))))))))

(defn roll-1cycle
  [mrocks maxX maxY]
  (-> mrocks
      (roll-dir maxX maxY "N")
      (roll-dir maxX maxY "W")
      (roll-dir maxX maxY "S")
      (roll-dir maxX maxY "E")
      ))

(defn North-load
  [maxY r-rocks] ;; rolling rocks
  (->> r-rocks
       (map #(- maxY (second %)) )
       (apply +)
       )
  )

(defn print-rocks
  [[maxX maxY] map-rocks]
  (let [Opos (set (map (fn[[rx ry _]] [rx ry]) (get map-rocks \O)))
        Fpos (set (map (fn [[rx ry _]] [rx ry]) (get map-rocks \#)))]
  (cljstr/join "\n" 
    (for [y (range maxY)]
    (apply str
      (for [x (range maxX)]
        (cond
          (contains? Opos [x y]) "O"
          (contains? Fpos [x y]) "#"
          :else "."
        )))
  ))
    )
  )

(defn find-stable-pos
  [map-rocks maxX maxY totsteps]
  (loop [hist {0 (set (get map-rocks \O))} 
         this-map-rocks map-rocks
         step 0]
    (let [new-map-rocks (roll-1cycle this-map-rocks maxX maxY)
          setO (set (get new-map-rocks \O))
        ;;   _ (newline)
        ;;   _ (prn (inc step))
        ;;   _ (println (print-rocks [maxX maxY] new-map-rocks))
          _ (when (= 0 (mod (inc step) 1)) (println "  step" (inc step)))
          ]
      (if (contains? (set (vals hist)) setO)
        (let [i1 (get (cljset/map-invert hist) setO)
            ;;   _ (prn i1)
              i2 (inc step)
            ;;   _ (prn i2)
              big-step (- i2 i1)
            ;;   _ (prn big-step)
              rep (quot (- totsteps i1) big-step)
            ;;   _ (prn rep)
              near-end (+ i1 (* rep  big-step))
            ;;   _ (prn near-end)
              end (+ i1 totsteps (- near-end))
            ;;   _ (prn end)
              _ (println "  found cycle :" i1 i2 "-->" end)
              ]
          (vec (get hist end))
          )
        (recur (assoc hist (inc step) setO) new-map-rocks (inc step))
        ))
    )
  )

(defn d14p2
  [input Nsteps]
  (let [[[maxX maxY] map-rocks] (parse-input input)]
    #_(roll-1cycle map-rocks maxX maxY)
    #_(print-rocks [maxX maxY] {\O (find-stable-pos map-rocks maxX maxY Nsteps) \# (get map-rocks \#)})
    (North-load maxY (find-stable-pos map-rocks maxX maxY Nsteps))
    ))


(defn d14
  [input]
  (let [[[maxX maxY] map-rocks] (parse-input input)]
    (->
     map-rocks
     roll-North
     (get \O)
     (->> (North-load maxY)))))


(defn -main
  [& args]
  (println "hello")
  (prn (d14 sample))
  #_(println (d14 (slurp "input/day14.txt")))
  (println sample)
  (newline)
  (println (d14p2 sample 1000000000))
  (println (d14p2 (slurp "input/day14.txt") 1000000000)))