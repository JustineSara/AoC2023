(ns t.d16
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample (slurp "input/day16sample"))

(defn parse-input
  [input]
  (->> 
    (map-indexed 
      (fn [y l] (map-indexed (fn [x c] [[x y] c]) l))
      (cljstr/split-lines input))
    (apply concat)
    (into {})
  ))

(def dic-dir 
  {[">" \.] [">"]
   [">" \-] [">"]
   [">" \|] ["^" "v"]
   [">" \/] ["^"]
   [">" \\] ["v"]
   
   ["<" \.] ["<"]
   ["<" \-] ["<"]
   ["<" \|] ["^" "v"]
   ["<" \/] ["v"]
   ["<" \\] ["^"]

   ["^" \.] ["^"]
   ["^" \-] ["<" ">"]
   ["^" \|] ["^"]
   ["^" \/] [">"]
   ["^" \\] ["<"]
   
   ["v" \.] ["v"]
   ["v" \-] ["<" ">"]
   ["v" \|] ["v"]
   ["v" \/] ["<"]
   ["v" \\] [">"]
   })

(defn ligh-move-right
  [[lx ly] mirrors]
  (let [newlx (inc lx)
        m (get mirrors [newlx ly])]
    (when m
      (map (fn [d] [newlx ly d]) (get dic-dir [">" m]))))
  )


(defn new-pos
  [x y d]
  [[(cond (= d ">") (inc x) (= d "<") (dec x) :else x)
    (cond (= d "v") (inc y) (= d "^") (dec y) :else y)]
   d]
  )

;; testing a tile is same as (contains? mirror [x y])

(defn test-not-in-past
  [[pos dir] past]
  (if (contains? past pos)
    (every? #(not= dir %) (get past pos))
    true)
  )


(defn light-move
  [all-lights past mirrors]
  ;; (newline)
  (if (zero? (count all-lights))
    past
    (let [[[lx ly] ldir] (first all-lights)
          all-lights (rest all-lights)
          ;; _ (prn lx ly ldir)
          m (get mirrors [lx ly])
          ;; _ (prn m)
          newdirs (get dic-dir [ldir m])
          newlights (map (fn [d] (new-pos lx ly d)) newdirs)
          ;; _ (prn newlights)          
          ;; _ (prn past)
          newlights (filter (fn [s] (test-not-in-past s past)) newlights)
          newlights (filter (fn [[pos _]] (contains? mirrors pos)) newlights)
          ;; _ (prn newdirs)
          ]
      #_(println "   [" lx ly ldir"]  " m " --> " newlights )
      (recur
       (concat all-lights newlights)
       (reduce #(update %1 (first %2) conj (second %2)) past newlights)
       mirrors
      )
      
    )
    
  ))


(defn d16 
  [input]
(let [mirrors (parse-input input)
      ]
  (newline)
  (->>
    (light-move [[[0 0] ">"]] {[0 0] [">"]} mirrors)
    keys
   count
   )
  ))


(defn d16p2
  [input]
  (let [mirrors (parse-input input)
        maxX (apply max (map first (keys mirrors)))
        maxY (apply max (map second (keys mirrors)))]
    (newline)
    (->>
     (concat (mapcat (fn[y] [[[0 y] ">"] [[maxX y] "<"]]) (range (inc maxY)))
             (mapcat (fn [x] [[[x 0] "v"] [[x maxY] "^"]]) (range (inc maxX))))
     (map
      (fn [start]
        (->>
         (light-move [start] {(first start) [(second start)]} mirrors)
         keys
         count))
      )
      (apply max)
     
     )
    )
  
  
  )


(defn -main
  [& args]
  (println "day16")
  (println sample)
  (prn (d16 sample))
  (newline)
  (println (d16 (slurp "input/day16.txt")))
  (newline)
  (prn (d16p2 sample))
  (newline)
  (println (d16p2 (slurp "input/day16.txt")))
)