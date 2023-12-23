(ns t.d23p2
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
       (filter (fn [[_ c]] (not= \# c)))
       (map (fn [[p c]] p))
       set
       ))

(defn one-step
  [[x y :as pos] forest]
    (->> 
     [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]
     (filter (fn [p] (contains? forest p)))
     ))

(def one-step-mm (memoize one-step))

(defn find-start-pos
  [forest]
  (->> forest
       (filter (fn [[_ y]] (= y 0)))
       (first)))

(defn find-end-pos
  [forest]
  (->> forest
       (filter (fn [[_ y]] (= y (apply max (map (fn [[_ y]] y) forest)))))
       (first)))


(defn one-step-all-paths
  [all-paths forest]
  ;; one path is {:visited [p1 p2 ..] :pos [x y]}
  (loop [paths-tbd all-paths
         paths-done []]
    (if (empty? paths-tbd)
      paths-done
      (let [[path & paths-tbd] paths-tbd
            pos (:pos path)
            new-pos (->> (one-step-mm pos forest)
                         (filter (fn [p] (not (contains? (:visited path) p)))))
            new-paths (map (fn [p] {:pos p :visited (conj (:visited path) p)}) new-pos)]
        (if (empty? new-pos)
          (recur paths-tbd paths-done)
          (recur paths-tbd (vec (concat paths-done new-paths))))))))

(defn search-longest-path
  [forest]
  (let [start (find-start-pos forest)
        end   (find-end-pos forest)]
    (loop [paths [{:pos start :visited #{start}}]
           best-path 0
           iter 0]
      #_(when (zero? (mod iter 100000))
        (println iter " :  best-path :" best-path "  count-path :" (count paths)))
      (if (empty? paths)
        best-path
        (let [[path & paths] paths
              pos (:pos path)
              new-pos (->> (one-step-mm pos forest)
                           (filter (fn [p] (not (contains? (:visited path) p)))))
              ended-path-length (if (empty? (filter (fn [p] (= p end)) new-pos)) 0 (inc (count (:visited path))))
              new-paths (->> new-pos
                             (filter (fn [p] (not= p end)))
                             (map (fn [p] {:pos p :visited (conj (:visited path) p)})))
              ]
          (when (> ended-path-length best-path) (println "  new longest :" ended-path-length "(in iter" iter ")"))
          (recur (concat new-paths paths) (max best-path ended-path-length) (inc iter)))))))

(defn path->segm
  [path forest]
  ;; return [ new-segm [paths-to-explored] ]
  (loop [pos (:start path)
         visited #{(:start path) (:from path)}]
    (let [next-pos (->> (one-step-mm pos forest)
                        (filter #(not (contains? visited %))))]
      (case (count next-pos)
        0 [ (assoc path :end pos :to [] :size (dec (count visited))) []]
        1 (recur (first next-pos) (conj visited (first next-pos)))
        [(assoc path :end pos :to next-pos :size (dec (count visited))) (map (fn [p] {:from pos :start p} ) next-pos) ]
        ;; note : we dec the visited because of the "from" who is included
        ))
    
    
    ))


(defn get-segments
  [forest]
  (let [start (find-start-pos forest)
        end   (find-end-pos forest)]
    (loop [segments []
           seg-to-explore [{:start start :from nil}]
           pos-considered #{start}
           ]
      (if (empty? seg-to-explore)
        segments
        (let [[path & seg-to-explore] seg-to-explore
              [new-seg more-to-explore] (path->segm path forest)
              more-to-explore (filter (fn [{s :start}] (not (contains? pos-considered s))) more-to-explore)
              ]
          (recur (conj segments new-seg) (concat seg-to-explore more-to-explore) (apply conj pos-considered (map :start more-to-explore)) ))
        )
     )
    )
)

(defn filter-segm
  [segments end]
  (->>
   segments
   (filter (fn [{sto :to e :end}] (or (> (count sto) 0) (= e end) )))
   )
)

(defn find-end-segm
  [segments end]
  (->>
   segments
   (filter (fn [{sto :to}] (zero? (count sto))))
   (filter (fn [{e :end}] (= e end)))
   (first)
   ))

(defn search-with-segm
  [forest]
  (let [segments (time (get-segments forest))
        end (find-end-pos forest)
        first-segms (filter (fn [{s :start}] (= s (find-start-pos forest))) segments)]
    ;; (println segments)
    ;; (filter-segm segments (find-end-pos forest))
    ;; (find-end-segm segments (find-end-pos forest))
    (loop [cur-segms (map #(assoc % :visited #{(:start %) (:end %)}) first-segms)
           max-path 0
           iter 0]
      (when (zero? (mod iter 1000000)) (println "iter:" iter " -  max:" max-path " - nsegm:" (count cur-segms) ))
      (if (empty? cur-segms) max-path
        (let [[segm & cur-segms] cur-segms
            ;;   _ (println segm)
              add-segm (->> segments
                            (filter (fn [{f :from}] (= f (:end segm))))
                            (filter (fn [{e :end}] (not (contains? (:visited segm) e)))))
            ;;   _ (println add-segm)
              new-segm (map (fn [s] {:from (:from segm)
                                     :start (:start segm)
                                     :end (:end s)
                                     :to (:to s)
                                     :size (+ (:size s) (:size segm))
                                     :visited (conj (:visited segm) (:start s) (:end s))}) add-segm)
            ;;   _ (println new-segm)
              ]
          (if (zero? (count add-segm))
            (if (= (:end segm) end) (recur cur-segms (max max-path (:size segm)) (inc iter)) (recur cur-segms max-path (inc iter)))
            (recur (concat new-segm cur-segms) max-path (inc iter)))
          )
      ))
))

;; this is a mistake the StackOverflow error came from lazy seq
;; loop is actually better than calling the same fct
#_(defn search-longest-path-p2
  [iter paths best-path forest end]
    (when (zero? (mod iter 100))
      (println iter " :  best-path :" best-path "  count-path :" (count paths)))
    (if (empty? paths)
        best-path
        (let [next-paths (one-step-all-paths paths forest)
              ended-paths-length (->> next-paths
                                      (filter (fn [{p :pos}] (= p end)))
                                      (map (fn [{v :visited}] (count v))))
              next-paths (filter (fn [{p :pos}] (not= p end)) next-paths)]
          (search-longest-path-p2
           (inc iter)
           next-paths 
           (apply max best-path ended-paths-length) 
           forest
           end))))


(defn d23
  [input]
  (let [forest (parse-input input)]
    ;; forest
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

    #_(dec (search-longest-path forest))
    ;; dec because we want the number of steps, not the number of tiles visited
    #_(dec (search-longest-path-p2
            0
            [{:pos (find-start-pos forest) :visited #{(find-start-pos forest)}}]
            0
            forest
            (find-end-pos forest)))

    (dec (search-with-segm forest))
    
    ;; looking at where are the intersections but did not help for ideas
    #_(->> forest
         get-segments
         (map :from)
         (set)
         sort
         )
    ))


(defn -main
  [& args]
  (println "day23")
  (println sample)
  (newline)

  (println "part2")
  (time (prn (d23 sample)))
  (time (prn (d23 (slurp "input/day23.txt"))))

  )
;; 5767 is too low
;; 5963 is also too low
;; 6595 is too high !!
(comment "
  new longest : 5767 (in iter 59702 )      --> too low
  new longest : 5859 (in iter 5774092 )
  new longest : 5943 (in iter 5863479 )
  new longest : 5963 (in iter 79152665 )   --> too low   
  new longest : 6087 (in iter 127514592 )
  new longest : 6115 (in iter 169160793 )
  new longest : 6123 (in iter 170779799 )
  new longest : 6143 (in iter 255089099 )
  new longest : 6215 (in iter 331059102 )
  new longest : 6235 (in iter 341433180 )
  new longest : 6247 (in iter 393281645 )
  new longest : 6451 (in iter 394223812 )
  new longest : 6471 (in iter 413272160 )
  new longest : 6595 (in iter 420220373 )  --> too high !! (yeah, I needed to aplpy the dec !)
          ")