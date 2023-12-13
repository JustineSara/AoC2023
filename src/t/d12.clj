(ns t.d12
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]
   [clojure.math :as maths]))



(def d12sample "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defn parse-line
  [line]
  (let [[sc nm] (cljstr/split line #" ")]
    [sc
     (map parse-long (cljstr/split nm #","))]))


(defn split-mixed
;;   enter a string of mixed # and ?
;;   outs a list of num starting from ? and ending with ? (can be 0)  thus ? # ? # ? ...
  [s]
  (let [m (map (fn [x] [(first x) (count x)]) (partition-by #(= \# %) s))
        m (if (= (ffirst m) \#) (concat [[0 \? 0]] m) m)
        m (if (= (first (last m)) \#) (concat m [["last" \? 0]]) m)]
    (map last m)
    ))

(defn type-piece-schem
  [s]
  (let [fq (frequencies s)
        c (count s)]
    (cond
      (= c (get fq \? 0)) {:type "qm" :n c}
      (= c (get fq \# 0)) {:type "ht" :n c}
      :else {:type "mixed" :n c :frequencies fq :splitted (split-mixed s)}
      )
    
    ))

(defn parse-schematics
  [sc]
  (let [sc-spt (cljstr/split sc #"\.+")]
    (map-indexed (fn [i s] (assoc (type-piece-schem s) :idx i :str s)) sc-spt)
    ))


(defn find-largest-htgroup
  [groups] ;; groups are sc-parsed or sub-groups of it
  (let [largest-ht-gp (first (sort-by #(:n %) > (filter #(= (:type %) "ht") groups)))
        first-rest-group (if largest-ht-gp
                          (filter #(< (:idx %) (:idx largest-ht-gp)) groups)
                          groups)
        last-rest-group  (when largest-ht-gp
                           (filter #(> (:idx %) (:idx largest-ht-gp)) groups))]
    [first-rest-group largest-ht-gp last-rest-group]))

(defn deal-with-htgroup
  [[gr-before ht-group gr-after] nm]
;;   returns [Ncases [list of pairs [groups nums] to be treated]]
  (if ht-group
    (let [n-ht (get ht-group :n)]
      (map (fn [[b a]] [1 [[gr-before b] [gr-after (rest a)]]]) (keep identity (map-indexed (fn [i n] (when (= n n-ht) (split-at i nm))) nm)))
      )
    nil)
  )


(defn deal-with-qmgroup-isfirst
  [[qm-group gr-after] nm]
;;   returns [[Ncases [list of pairs [groups nums] to be treated]]
  (let [n-qm (get qm-group :n)]
    (loop [i 1 res [[1 [gr-after nm]]]]
      (if (> i (count nm))
        res
        (let [sub-nm (first (split-at i nm))
              ntot (+ (apply + sub-nm) (dec i))
              ]
          (if
            (< n-qm ntot)
            res
            (recur (inc i) (conj res [(quot (apply * (range (inc (- n-qm ntot)) (+ (- n-qm ntot) (inc i)))) (apply * (range 1 (inc i))) )
                                      [gr-after (rest (split-at i nm))]]))
            ;; (if (= i 1) (+ n-qm (- ntot) 1)     )
            ;; (or (= 1 i) (= ntot n-qm)) (recur (inc i) (conj res [(+ n-qm (- ntot) 1) [gr-after (rest (split-at i nm))]]))
            ;; :else
            ;; (recur (inc i) (conj res [ (+ n-qm (- ntot) 1 (* (maths/pow (dec i) (- n-qm ntot)) (apply * (range 1 (+ n-qm (- ntot) 2)))))
            ;;                           [gr-after (rest (split-at i nm))] ]))
            )
            )
      )
     ))
)


(defn deal-with-mixedgroup-isfirst
  [[mixed-group gr-after] nm]
;;  mixed-group has :splitted a list of N? N# N? N# ...
  (let [n (first nm)
        splitted (get mixed-group :splitted)
        N? (first splitted)
        N# (second splitted)
        N?2 (nth splitted 3)]
    (cond
      (< n N?) (deal-with-qmgroup-isfirst {:n (dec N?)} nm)
      (<= n (+ N? N#)) (println "case 1 ")
      )

    )
  
  )



(defn d12
  []
  (let [input d12sample
        lines  (cljstr/split-lines input)]
    (for [l lines]
      (let [[sc nm] (parse-line l)
            sc-parsed (parse-schematics sc)]
        
        (println sc)
        (println nm)
        (println " " sc-parsed)
        (println "   " (find-largest-htgroup sc-parsed))
        (when (second (find-largest-htgroup sc-parsed))
          (println "     " (deal-with-htgroup (find-largest-htgroup sc-parsed) nm)))
        ;; (println "   " (map type-piece-schem sc-parsed))
        
      )
    )

    (println [{:type "qm", :n 24, :idx 1, :str "10?"} ()] (repeat 10 1))
    (println (deal-with-qmgroup-isfirst [{:type "qm", :n 24, :idx 1, :str "10?"} ()] (repeat 10 1)))

  ))

(defn -main
  [& args]
  (println "hello")
  (println (d12))
  )


