(ns t.d15
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))


(def sample "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")
(defn op-on-c
  [prev c]
  (->
   c
   .hashCode
   (+ prev)
   (* 17)
   (mod 256)
   ))

(defn d15
  [input]
  (->
   input
   (cljstr/split-lines)
   first
   (cljstr/split #",")
   (->> (map (fn [l] (reduce op-on-c 0 l)))
        (apply +))
  )
  )


(defn insert
  [boxes lense]
  (let [thisbox (get boxes (get lense :box) [])
        splitted (split-with (fn [l] (not= (get lense :label) (get l :label))) thisbox)
        p1 (first splitted)
        p2 (rest (second splitted))]
    (assoc boxes (get lense :box) (concat p1 [lense] p2)))
  )

(defn out
  [boxes lense]
  (let [thisbox (get boxes (get lense :box) [])
        splitted (split-with (fn [l] (not= (get lense :label) (get l :label))) thisbox)
        p1 (first splitted)
        p2 (rest (second splitted))]
    (assoc boxes (get lense :box) (concat p1 p2))))


(defn parse-lense
  [l]
  (let [mtch (re-find #"(.*)(-|=)(\d*)" l)]
    (if (= (nth mtch 2) "=")
      {:label (second mtch)
       :box (reduce op-on-c 0 (second mtch))
       :operation insert
       :focal (parse-long (nth mtch 3))}
      {:label (second mtch)
       :box (reduce op-on-c 0 (second mtch))
       :operation out}
      )))

(defn focusing-power
  [l slot]
  (* 
   (inc (:box l))
   slot
   (:focal l)))

(defn d15p2
  [input]
  (->
   input
   (cljstr/split-lines)
   first
   (cljstr/split #",")
   (->> (map parse-lense)
        
        (reduce (fn [b l] ((get l :operation) b l)) {})
        (vals)
        (map (fn [listl] (map focusing-power listl (range 1 (inc (count listl))))) )
        (apply concat)
        (apply +)
        )
   )
  
  #_(insert {1 [{:label "d"} {:label "aa"} {:label "label"} {:label "f"}]} {:label "aa" :box 1})
  #_(out {1 [{:label "d"} {:label "aa"} {:label "label"} {:label "f"}]} {:label "aa" :box 1})
  )


(defn -main
  [& args]
  (println "day15")
;;   (println "HASH")
;;   (prn (d15 "HASH"))
;;   (newline)
;;   (println sample)
;;   (prn (d15 sample))
;;   (newline)
;;   (println (d15 (slurp "input/day15.txt")))
;;   (newline)
  
  (newline)
  (println "part 2")
  (println sample)
  (prn (d15p2 sample))
  (newline)
  (println (d15p2 (slurp "input/day15.txt")))
  )