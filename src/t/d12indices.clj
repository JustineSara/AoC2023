(ns t.d12indices
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]
   [clojure.math :as maths]))


(comment
"
???..### 1, 1, 3
1 : 0 1 2
3 : 0 5
cases
0 2 5 

0123456789
???..###.??? 1.1.3
1: 0 1 2 9 10 11
3: 0 5 9
case
0 1 2 3 5 8
0 1 2 3 9 <- faux
"
)





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


(defn num-indices
  [nm sc]
  ;; (newline)
  ;; (println nm "  " sc)
  (let [sc (str "s" sc "e")
        p (re-pattern (str "[s|.|?][?|#]{" nm "}[e|.|?]"))
        ;; _ (println "  " p)
        ]
    (loop [s sc 
           all-i [] 
           offset-i -1]
      (let [s-found (re-find p s)
            i-found (when s-found (cljstr/index-of s s-found))
            ;; _ (println "     " i-found "  " s-found)
            ]
        (if i-found
          (let [actual-i (+ i-found 1 offset-i)]
            (recur (subs s (inc i-found)) (conj all-i actual-i) actual-i))
          all-i
          )
        )
      )
    )
  )

(defn ht-indices
  [sc]
  (keep identity (map-indexed (fn [i c] (when (= c \#) i)) sc))
  )

(defn check-ht
  [serie-of-ind ht-ind]
  (let [tbtested (filter #(<= % (apply max serie-of-ind)) ht-ind)]
    (every? #(> (count %) 0)
      (map 
        (fn [i] (filter #(<= (first %) i (dec (second %))) (partition 2 serie-of-ind)) ) 
        tbtested))
    )
  )

(defn combine-ind
  [nm num-ind ht-ind]
  (loop [nums nm 
         inprogress [[]]]
    (if (zero? (count nums))
      (count inprogress)
      (let [[n & nums] nums
            ;; _ (prn n nums)
            n-ind (get num-ind n)
            ;; _ (prn n-ind)
            inprogress (mapcat 
                        (fn [op] (map #(concat op [% (+ % n)]) (filter #(< (apply max -1 op) %) n-ind)))
                        inprogress)
            ;; _ (prn inprogress)
            ]
        (recur nums (filter #(check-ht % ht-ind) inprogress))
        )
      )
    )
  )

#_(defn combine-ind
  [nm num-ind ht-ind]
  (let [list-ind (map #(get num-ind %) nm)
        tobedone [list-ind]]
    (loop [tbd tobedone
           N 0]
      (if (zero? (count tbd))
        N
        (let [[this-step & tbd] tbd
              [fixed_n tbfixed] (split-with #(not (coll? %)) this-step)]
          (if (zero? (count tbfixed))
            (recur tbd (inc N))
            (let [[next-ind & rest-ind] tbfixed
                  [nextnum & rest-num] next-ind]
              (if (zero? (count rest-num))
                ()
                ()
                )

              )
            )
          ))
      )
    
    )
  )

(defn select-best-split
  [nm num-ind]
  (let [nm-countind (map #(count (get num-ind %)) nm) 
        least-choices (apply min nm-countind)
        indxB  (keep identity (map-indexed (fn [i c] (when (= c least-choices) i)) nm-countind))
        indxB (nth indxB (quot (count indxB) 2))]
    [indxB (nth nm indxB)]
    )
  )

(defn recur-from-bestsplit
  [nm num-ind ht-ind min-ind max-ind]
  (let [[idB nmB] (select-best-split nm num-ind)
        [before [_ & after]] (split-at idB nm)
        nmindB (filter #(<= min-ind % (+ % nmB) max-ind) (get num-ind nmB))
        ]
    (cond
      (and (empty? before) (empty? after))
      (count nmindB)
      (empty? before)
      (map (fn [i] (recur-from-bestsplit after num-ind ht-ind (+ i nmB 1) max-ind)) nmindB)
      (empty? after)
      (map (fn [i] (recur-from-bestsplit before num-ind ht-ind min-ind (dec i))) nmindB)
      :else
      (mapcat (fn [i] [(recur-from-bestsplit before num-ind ht-ind min-ind (dec i)) 
                       (recur-from-bestsplit after  num-ind ht-ind (+ i nmB 1) max-ind)]) nmindB)
      )
    )
  )

(defn one-line-sol
  [l]
  (let [[sc nm] (parse-line l)
        _ (println "  " sc "  " nm)
        num-ind (into {} (map (fn [n] [n (num-indices n sc)]) (set nm)))
        ht-ind (ht-indices sc)]
    (combine-ind nm num-ind ht-ind)
    ))

(defn part2-sol
  [l]
  (let [[sc nm] (parse-line l)
        sc (cljstr/join "?" (repeat 5 sc))
        nm (apply concat (repeat 5 nm))
        _ (println "  " sc "  " nm)
        num-ind (into {} (map (fn [n] [n (num-indices n sc)]) (set nm)))
        ht-ind (ht-indices sc)]
    (combine-ind nm num-ind ht-ind)))

(defn d12
  [input]
  (let [lines  (cljstr/split-lines input)
        extra-test "???.###.??? 1,1,3"
        [sc-extra nm-extra] (parse-line extra-test)
        num-ind (into {} (map (fn [n] [n (num-indices n sc-extra)]) (set nm-extra)))
        ht-ind (ht-indices sc-extra)
        ]

    #_(apply 
     + 
     (map one-line-sol lines))

    (newline)
    (println sc-extra)
    (println nm-extra)
    (println num-ind)
    (println ht-ind)
    (println (recur-from-bestsplit nm-extra num-ind ht-ind 0 (count sc-extra)))
    ;; (println (combine-ind nm-extra num-ind ht-ind))

    #_(for [l lines]
      (do
      (newline)
      (println l)
      (println (one-line-sol l))
      ;; (println (part2-sol l))
      ))
))

(defn -main
  [& args]
  (println "hello")
  (println (d12 d12sample))
  ;; (println (d12 (slurp "input/day12.txt")))
  )