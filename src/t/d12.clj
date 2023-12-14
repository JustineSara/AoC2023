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
    (map last m)))
    

(defn type-piece-schem
  [s]
  (let [fq (frequencies s)
        c (count s)]
    (cond
      (= c (get fq \? 0)) {:type "qm" :n c}
      (= c (get fq \# 0)) {:type "ht" :n c}
      :else {:type "mixed" :n c :frequencies fq :splitted (split-mixed s)})))
      
    
    

(defn parse-schematics
  [sc]
  (let [sc-spt (cljstr/split sc #"\.+")]
    (map-indexed (fn [i s] (assoc (type-piece-schem s) :idx i :str s)) sc-spt)))
    


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
      (map (fn [[b a]] [1 [[gr-before b] [gr-after (rest a)]]]) (keep identity (map-indexed (fn [i n] (when (= n n-ht) (split-at i nm))) nm))))
      
    nil))
  


(defn deal-with-qmgroup-isfirst
  [[qm-group gr-after] nm]
;;   returns [[Ncases [list of pairs [groups nums] to be treated]]
  (let [n-qm (get qm-group :n)]
    (loop [i 1 res [[1 [gr-after nm]]]]
      (if (> i (count nm))
        res
        (let [sub-nm (first (split-at i nm))
              ntot (+ (apply + sub-nm) (dec i))]
              
          (if
            (< n-qm ntot)
            res
            (recur (inc i) (conj res [(quot (apply * (range (inc (- n-qm ntot)) (+ (- n-qm ntot) (inc i)))) (apply * (range 1 (inc i))))
                                      [gr-after (rest (split-at i nm))]]))))))))
            ;; (if (= i 1) (+ n-qm (- ntot) 1)     )
            ;; (or (= 1 i) (= ntot n-qm)) (recur (inc i) (conj res [(+ n-qm (- ntot) 1) [gr-after (rest (split-at i nm))]]))
            ;; :else
            ;; (recur (inc i) (conj res [ (+ n-qm (- ntot) 1 (* (maths/pow (dec i) (- n-qm ntot)) (apply * (range 1 (+ n-qm (- ntot) 2)))))
            ;;                           [gr-after (rest (split-at i nm))] ]))
            
            
      
     



(defn deal-with-mixedgroup-isfirst
  [[mixed-group gr-after] nm]
;;  mixed-group has :splitted a list of N? N# N? N# ...
  (let [n (first nm)
        splitted (get mixed-group :splitted)
        first-fit (first (filter (fn [spl] (<= n (dec (apply + spl)))) (map (fn[i] (take i splitted)) (range 1 (inc (count splitted)) 2))))
        lenff (count first-fit)
        _ (prn first-fit)]
    
    (cond
      (zero? lenff) nil
      (= 1 lenff)
    ;;   (deal-with-qmgroup-isfirst [{:n (dec (first first-fit))} () ] nm) 
             ;; for solutions ending in # then [0 special-rest-splitted]
             ;; for solutions ending in . then [1 special-rest-splitted]
             ;;  ACTUALLY : should do it recursively
    ;;   TODO tout doux : turn this into same format as output or deal-with-qmgroup... [ [ncases [ [list of groups after] (list of number to put into) ]] ]
      (concat [[1 (concat [0] (rest splitted)) nm]] (map (fn [next-n?] [1 (concat [next-n?] (rest splitted)) (rest nm)] ) (range 0 (- (first first-fit) n 1))))
    ;;   (<= n (+ N? N#)) (if (< n N#)
    ;;                      nil
    ;;                      (println "case 1 call on [(dec N?2) (rest rest rest splitted)] and (rest nm)")
    ;;                      )
    ;;   NEXT : DO the condition where more than the question mark :
    ;;           1. need to calculate the number of fixed # 
    ;;                 by taking the sum between first and last excluded (rest (drop-last first-fit))
    ;;           2. many conditions : if n < fixed# ==> nil
    ;;                                if   =        ==> 1 and next sequence with last-1 ? (as start of next seq)
      )))
      
      

    
  
  



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
          (println "     " (deal-with-htgroup (find-largest-htgroup sc-parsed) nm)))))
        ;; (println "   " (map type-piece-schem sc-parsed))
        
      
    

    (println [{:type "qm", :n 24, :idx 1, :str "10?"} '({:type "gr-after"})] (repeat 10 1))
    (println (deal-with-qmgroup-isfirst [{:type "qm", :n 24, :idx 1, :str "10?"} '({:type "gr-after"})] (repeat 10 1)))
    
    (newline)
    
    (println [{:type "mixed", :n 7, :idx 1, :str "?#?#?#?" :splitted '(1 1 1 1 1 1 1)} '({:type "gr-after"})] '(3))
    (deal-with-mixedgroup-isfirst [{:type "mixed", :n 7, :idx 1, :str "?#?#?#?" :splitted '(1 1 1 1 1 1 1)} '({:type "gr-after"})] '(3))
    
    (newline)
    
    (println [{:type "mixed", :n 7, :idx 1, :str "?????#" :splitted '(5 1 0)} '({:type "gr-after"})] '(1 2 3))
    (deal-with-mixedgroup-isfirst [{:type "mixed", :n 7, :idx 1, :str "?????#" :splitted '(5 1 0)} '({:type "gr-after"})] '(1 2 3))

    ))
  

(defn -main
  [& args]
  (println "hello")
  (println (d12)))
  


