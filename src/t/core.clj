(ns t.core
  (:gen-class)
  (:require
    [clojure.string :as cljstr]
    [clojure.set :as cljset]))




(def d12sample1 "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defn d12p1parseinput
  [line]
  (let [[sc nm] (cljstr/split line #" " )]
    [sc
     (map parse-long (cljstr/split nm #","))])
  )

;; (defn neednotsplit?
;;   [sc nm]
;;   (= (count sc) (count nm)))

;; (defn isgood?
;;   [sc nm]
;;     (and
;;      (neednotsplit? sc nm)
;;      (every? true? (map (fn [sc1 nm1] (= (count (re-seq #"#" sc1)) nm1)) sc nm))))

(defn isgood?
  [sc nm]
  (let [fq (frequencies sc)
        Nht (get fq \# 0)
        Nqm (get fq \? 0)
        scsplit (filter (fn [x] (cljstr/index-of x "#")) (cljstr/split sc #"\.+"))]
    (cond
      (> Nqm 0) false
      (not= Nht (apply + nm)) false
      :else (every? true? (map (fn [sc1 nm1] (= (count sc1) nm1)) scsplit nm)))
    )
)

;; (defn N-missing-ht
;;   [sc nm]
;;   (- ))

(defn getallop
  [sc nm]
  (let [totht (apply + nm)]
    (loop [allop [sc]]
      (prn allop)
      (if (zero? (count (re-seq #"\?" (first allop))))
        allop
        (let [op1 (cljstr/replace-first (first allop) #"\?" ".")
              op2 (cljstr/replace-first (first allop) #"\?" "#")
              allop (rest allop)
              allop (if (zero? (count allop)) [] allop)]
          (if (> (get (frequencies op2) \# 0) totht)
            (recur (conj allop op1))
            (recur (conj allop op1 op2))
          ))))
  ))

(defn bruteforce
  [il line]
  (when (zero? (mod il 10)) (prn il))
  (let [[sc nm] (cljstr/split line #" ")
        nm (map parse-long (cljstr/split nm #","))
        allop (getallop sc nm)]
    (count (filter (fn [op] (isgood? (filter #(> (count %) 0) (cljstr/split op #"\.+")) nm)) allop))))
  
(defn ispotentialok?
  [sc nm]
  (let [Nht (get (frequencies sc) \# 0)
        totht (apply + nm)
        Npieces (count (filter (fn [x] (cljstr/index-of x "#")) (cljstr/split sc #"\.+")))
        biggestpiece (apply max (map count (clojure.string/split sc #"\.+")))]
    (cond
      (> Nht totht) false
      (> Npieces (count nm)) false
      (< biggestpiece (apply max nm)) false
      :else true
    )
  ))

(defn getgoodop
  [sc nm]
    (loop [tobetested [sc] goodone []]
      (if (zero? (count tobetested))
        goodone
        (let [t (first tobetested)
              tobetested (rest tobetested)]
          (cond
            (isgood? t nm) (recur tobetested (conj goodone t))
            (cljstr/index-of t "?") 
            (let [op1 (cljstr/replace-first t #"\?" ".")
                  op2 (cljstr/replace-first t #"\?" "#")]
              (cond
                (and (ispotentialok? op1 nm) (ispotentialok? op2 nm)) (recur (concat [op1 op2] tobetested) goodone)
                (ispotentialok? op1 nm) (recur (concat [op1] tobetested) goodone)
                (ispotentialok? op2 nm) (recur (concat [op2] tobetested) goodone)
                :else (recur tobetested goodone)
                ))
            :else (recur tobetested goodone)
            )
          ))
    ))

(defn bruteforce
  [il line]
  (when (zero? (mod il 10)) (prn il))
  (let [[sc nm] (cljstr/split line #" ")
        nm (map parse-long (cljstr/split nm #","))
        goodop (getgoodop sc nm)]
    (count goodop)))


(defn d12part1
  [input]
  (let [lines (cljstr/split-lines input)]
    (apply + (map-indexed bruteforce lines))
    ;; (bruteforce 0 (first lines))
    ;; (let [[sc nm] (str/split (nth lines 3) #" ")
    ;;       nm (map parse-long (str/split nm #","))]
    ;;   (ispotentialok? sc nm))
    ;; (getgoodop "?###????????" [3,2,1])
    ))

(defn refine-line-v2
  [line goodop]
  (let [[sc nm] (cljstr/split line #" ")
        nm (map parse-long (cljstr/split nm #","))]
  (apply str (apply map (fn [& all] (if (apply = all) (first all) \?)) goodop))
))

(defn d12part2-oneline
  ([line]
  (let [[sc nm] (cljstr/split line #" ")
        nm (map parse-long (cljstr/split nm #","))
        c  (count (getgoodop sc nm))
        c? (count (getgoodop (str sc \?) nm))
        cc (count (getgoodop (str sc \? sc) (concat nm nm)))
        diff (- c? c)
        div (quot cc c?)]
    (cond 
      (= cc 1) 1
      (zero? diff) (* c (apply * (repeat 4 div)))
      (not= cc (* c c?)) (count (getgoodop (str sc \? sc \? sc \? sc \? sc) (apply concat (repeat 5 nm))))
      :else (* c (apply * (repeat 4 c?)))
      )
    ))
  ([il line]
   (when (zero? (mod il 1)) (println il))
   (d12part2-oneline line)
   ))

(defn d12part2
  [input]
  (let [lines (cljstr/split-lines input)]
    ;; (map #(apply getgoodop (d12p1parseinput %)) lines)
    
    #_(for [l lines]
      (do 
      (newline)
      (prn l)
      (prn (d12part2-oneline l)))
        ;; (println "number of options:" (count goodop))
        ;; (println "N opts 4 [init]? :" (count (getgoodop (str sc \?) nm)))
        ;; (println "   [init]?[init] :" (count (getgoodop (str sc \? sc) (concat nm nm))))
        ;; (prn nl)
        ;; (cond
        ;;   (and (= (first nl) \#) (= (last nl) \#)) (apply * (repeat 5 (count goodop)))
        ;;   (and (= (first nl) \#) (= (last nl) \.)) (do (println " " (count (getgoodop (str \? sc) nm))))
          )
    (apply + (map-indexed d12part2-oneline lines))
    )
  #_(d12part2-oneline "???? 1,1" )
      )


  ;;   ;; (println sc nm)
  ;;   ;; ;; (prn (last sc))
  ;;   ;; ;; (cond
  ;;   ;; ;;   (= (last sc) \#)
  ;;   ;; ;;   (println "if last element is a # then the added ? must be a dot "))
  ;;   ;; ;; (newline)
  ;;   ;; (prn (refine-line l))
  ;;   ;; (prn (map refine-line lines))
  ;;   #_(let [c    (count (getgoodop sc nm))
  ;;         c+dt (count (getgoodop ".??..??...?##.." [1,1,3]))
  ;;         c+ht (count (getgoodop ".??..??...?##.#" [1,1,3]))
  ;;         dt+c (count (getgoodop "..??..??...?##." [1,1,3]))
  ;;         ht+c (count (getgoodop "#.??..??...?##." [1,1,3]))
  ;;         c+?+c (count (getgoodop ".??..??...?##.?.??..??...?##." [1,1,3,1,1,3]))]
  ;;     (println "original count" c)
  ;;     (println "with . " c+dt dt+c (* c+dt dt+c))
  ;;     (println "with # " c+ht ht+c (* c+ht ht+c))
  ;;     (println "tot . and # " (+ (* c+dt dt+c) (* c+ht ht+c)))
  ;;     (println "tot by search" c+?+c)
      
  ;;   )
  ;; ))

(defn find-all-indices
  [s p]

  ;; the full input is padded
  ;; (prn s)
  (let [f (re-find p s)
        i-padded (cljstr/index-of s f)
        i (dec i-padded)
        i-ht (inc i)]
    ;; (prn f)
    ;; (prn i-padded)
    ;; (prn i)
    ;; (prn (subs s (inc i-padded)))
    
    (loop [ss (subs s (inc i-padded)) indexes [i-ht] this-i i-padded]
      (let [f (re-find p ss)]
        (if
          f
          (let [i (cljstr/index-of ss f)]
          ;; (newline)
          ;; (println "   " f)
          ;; (println "   " i)
          ;; (println "   " this-i)
          ;; (println "   " (+ i this-i 1))
            (recur (subs ss (inc i)) (conj indexes (+ i this-i 1)) (+ this-i i 1)))
        indexes)))))

(defn combine-indices
  [options [nm nm-i]]
  ;; (println "  options" options)
  ;; (println "  nm     " nm)
  ;; (println "  nm-i   " nm-i)
  (apply concat 
    (for [op options]
      (map #(concat op [% (+ % nm)]) (filter #(< (apply max op) %) nm-i))
    )
))

(defn alltogether
  ([l]
    (let [[sc nm] (d12p1parseinput l)
          nm-re (map (fn [n] [n (re-pattern (str "[s|\\.|\\?][\\?|\\#]{" n "}[e|\\.|\\?]"))]) (set nm))
          nm-indices (into {} (map (fn [[n p]] [n (find-all-indices (str "s" sc "e") p)]) nm-re))
          nm-map (map (fn [n] [n (get nm-indices n)]) nm)]
      (count (reduce combine-indices [[-1]] nm-map))
        ))
   ([sc nm]
     (let [nm-re (map (fn [n] [n (re-pattern (str "[s|\\.|\\?][\\?|\\#]{" n "}[e|\\.|\\?]"))]) (set nm))
           nm-indices (into {} (map (fn [[n p]] [n (find-all-indices (str "s" sc "e") p)]) nm-re))
           nm-map (map (fn [n] [n (get nm-indices n)]) nm)]
       (count (reduce combine-indices [[-1]] nm-map)))))

(defn alltogether5
  [il l]
  (prn il)
  (prn "start fct")
  (let [[sc nm] (d12p1parseinput l)
        n0 (alltogether sc nm)
        n? (alltogether (str sc \?) nm)
        ?n (alltogether (str \? sc) nm)
        n?n (alltogether (str sc \? sc) (concat nm nm))]
    (prn "after let 1")
    (prn n0 n? ?n n?n)
    (cond
      (= (* n0 n?) n?n) (* n0 n? n? n? n?)
      (= (* n0 ?n) n?n) (* n0 ?n ?n ?n ?n)
      :else
      (let [sc (str sc \? sc \? sc \? sc \? sc)
            nm-re (map (fn [n] [n (re-pattern (str "[s|\\.|\\?][\\?|\\#]{" n "}[e|\\.|\\?]"))]) (set nm))
            nm-indices (into {} (map (fn [[n p]] [n (find-all-indices (str "s" sc "e") p)]) nm-re))
            nm-map (map (fn [n] [n (get nm-indices n)]) (apply concat (repeat 5 nm)))]
        (prn "after let 2")
        (count (reduce combine-indices [[-1]] nm-map)))
      )))


(defn urg
  [l]
  (let [[sc nm] (cljstr/split l #" ")
        nm1 (cljstr/replace nm #"\d" #(str "[\\?|\\#]{" %1 "}"))
        nm2 (cljstr/replace nm1 "," (str "[\\?|\\.]+"))
        p (re-pattern (str "[\\?|\\.]*" nm2 "[\\?|\\.]*"))
        fnm1 (cljstr/replace nm #"\d" #(str "\\#{" %1 "}"))
        fnm2 (cljstr/replace fnm1 "," (str "\\.+"))
        fp (re-pattern (str "^\\.*" fnm2 "[$|\\.]"))]
  (loop [scr [sc]
          Nvalid 0]
     (if
      (zero? (count scr))
       Nvalid
       (let [t (first scr)
             scr (rest scr)
             t1 (cljstr/replace-first t "?" ".")
             t2 (cljstr/replace-first t "?" "#")
             f2 (re-find p t2)
             f1 (re-find p t1)]
         (cond
           (re-find fp t) (recur scr (inc Nvalid))
           (zero? (get (frequencies t) \? 0)) (recur scr Nvalid)
           (and f1 f2) (recur (concat [t1 t2] scr) Nvalid)
           f1 (recur (concat [t1] scr) Nvalid)
           f2 (recur (concat [t2] scr) Nvalid)
           :else (recur scr Nvalid)))))))

(defn d12-with-re
  [input]

  (let [lines (cljstr/split-lines input)]
    (map urg lines)
    )
  ;; (map alltogether (cljstr/split-lines input))
  #_(let [sc (str "?###????????" \?)
        nm [3 2 1]
        nm-re (map (fn [n] [n (re-pattern (str "[s|\\.|\\?][\\?|\\#]{" n "}[e|\\.|\\?]"))]) (set nm))
        nm-indices (into {} (map (fn [[n p]] [n (find-all-indices (str "s" sc "e") p)]) nm-re))
        nm-map (map (fn [n] [n (get nm-indices n)]) nm)]
    (prn sc)
    (prn nm-indices)
    (prn (reduce combine-indices [[-1]] nm-map)))
  ;; (map-indexed alltogether5 (cljstr/split-lines input))
    
    
  #_(let [lines (cljstr/split-lines input)
        l (second lines)
        ;; [sc nm] (d12p1parseinput l)
        ;; nm-re (map (fn [n] [n (re-pattern (str "[s|\\.|\\?][\\?|\\#]{" n "}[e|\\.|\\?]"))]) (set nm))
        ;; nm-indices (into {} (map (fn [[n p]] [n (find-all-indices (str "s" sc "e") p)]) nm-re))
        ;; nm-map (map (fn [n] [n (get nm-indices n)]) nm)
        [sc nm] (cljstr/split l #" ")
        nm1 (cljstr/replace nm #"\d" #(str "[\\?|\\#]{" %1 "}"))
        nm2 (cljstr/replace nm1 "," (str "[\\?|\\.]+"))
        p (re-pattern (str "[\\?|\\.]*" nm2 "[\\?|\\.]*"))
        fnm1 (cljstr/replace nm #"\d" #(str "\\#{" %1 "}"))
        fnm2 (cljstr/replace fnm1 "," (str "\\.+"))
        fp (re-pattern (str "^\\.*" fnm2 "[$|\\.]" ))
        ]
    ;; (prn l)
    ;; (prn nm-indices)
    ;; (prn nm-map)
    ;; (prn (reduce combine-indices [[-1]] nm-map))
    ;; (prn (alltogether l))

    ;; (prn nm1)
    ;; (prn nm2)
    ;; (prn p)
    ;; (prn fp)
    ;; (prn (re-find p (subs sc 1)))
    #_(loop [scp sc 
           allpos []
           current-i 0]
      (println "  " scp)
      (println "  " allpos)
      (println "  " current-i)
      (let [f (re-find p scp)]
        (if f
          (let [i (cljstr/index-of scp f)](recur (subs scp (inc i)) (conj allpos (+ current-i i)) (+ current-i i 1)))
          allpos)))
    ;; the bottom loop works but so much the same as before except using regex to detect if good or bad
    ;; might be faster but I'm not so sure
    (loop [scr [sc]
           Nvalid 0]
      (println "  " Nvalid "  " (count scr))
      (println "  " (first scr))
      (if
        (zero? (count scr)) 
        Nvalid
        (let [t (first scr)
              scr (rest scr)
              t1 (cljstr/replace-first t "?" ".")
              t2 (cljstr/replace-first t "?" "#")
              f2 (re-find p t2)
              f1 (re-find p t1)]
          (cond 
            (re-find fp t) (recur scr (inc Nvalid))
            (zero? (get (frequencies t) \? 0)) (recur scr Nvalid)
            (and f1 f2) (recur (concat [t1 t2] scr) Nvalid)
            f1 (recur (concat [t1] scr) Nvalid)
            f2 (recur (concat [t2] scr) Nvalid)
            :else (recur scr Nvalid))))
    ))
)



;; d12 after 10PM -- I try to clean up a little

(defn bruteforcewithre
  [sc pattern-for-potential pattern-for-final-with-se]
  (loop [pot-sc [sc] Nvalid 0]
    (if
      (zero? (count pot-sc))
      Nvalid
      (let [t (first pot-sc)
            scr (rest pot-sc)
            ]
         (if
           (zero? (get (frequencies t) \? 0))
           (if
             (re-find pattern-for-final-with-se (str "s" t "e"))
             (recur scr (inc Nvalid))
             (recur scr Nvalid))
           (let [t1 (cljstr/replace-first t "?" ".")
                 t2 (cljstr/replace-first t "?" "#")
                 f1 (re-find pattern-for-potential t1)
                 f2 (re-find pattern-for-potential t2)]
             (cond 
               (and f1 f2) (recur (concat [t1 t2] scr) Nvalid)
               f1 (recur (concat [t1] scr) Nvalid)
               f2 (recur (concat [t2] scr) Nvalid)
               :else (recur scr Nvalid))))
      )
   )

  ))


(defn fromline-bruteforcewithre
  ([l]
  (let [[sc nm] (d12p1parseinput l)
        pattern-for-potential (re-pattern (str "[\\?|\\.]*" (cljstr/join "[\\?|\\.]+" (map #(str "[\\?|\\#]{" % "}") nm)) "[\\?|\\.]*"))
        pattern-for-final-with-se (re-pattern (str "s\\.*" (cljstr/join "\\.+" (map #(str "\\#{" % "}") nm)) "\\.*e"))]
    (bruteforcewithre sc pattern-for-potential pattern-for-final-with-se)
  ))
  ([il l]
   (prn il)
   (let [[sc nm] (d12p1parseinput l)
         pattern-for-potential (re-pattern (str "[\\?|\\.]*" (cljstr/join "[\\?|\\.]+" (map #(str "[\\?|\\#]{" % "}") nm)) "[\\?|\\.]*"))
         pattern-for-final-with-se (re-pattern (str "s\\.*" (cljstr/join "\\.+" (map #(str "\\#{" % "}") nm)) "\\.*e"))]
     (bruteforcewithre sc pattern-for-potential pattern-for-final-with-se))))

(defn fromline-bruteforcewithre-factor5
  ([l]
   (let [[sc nm] (d12p1parseinput l)
         pattern-for-potential (re-pattern (str "[\\?|\\.]*" (cljstr/join "[\\?|\\.]+" (map #(str "[\\?|\\#]{" % "}") nm)) "[\\?|\\.]*"))
         pattern-for-final-with-se (re-pattern (str "s\\.*" (cljstr/join "\\.+" (map #(str "\\#{" % "}") nm)) "\\.*e"))
         N1 (bruteforcewithre sc pattern-for-potential pattern-for-final-with-se)
         sc2 (str sc \? sc)
         pattern-for-potential2 (re-pattern (str "[\\?|\\.]*" (cljstr/join "[\\?|\\.]+" (map #(str "[\\?|\\#]{" % "}") (apply concat (repeat 2 nm)))) "[\\?|\\.]*"))
         pattern-for-final-with-se2 (re-pattern (str "s\\.*" (cljstr/join "\\.+" (map #(str "\\#{" % "}") (apply concat (repeat 2 nm)))) "\\.*e"))
         N2 (bruteforcewithre sc2 pattern-for-potential2 pattern-for-final-with-se2)
         sc3 (str sc2 \? sc)
         pattern-for-potential3 (re-pattern (str "[\\?|\\.]*" (cljstr/join "[\\?|\\.]+" (map #(str "[\\?|\\#]{" % "}") (apply concat (repeat 3 nm)))) "[\\?|\\.]*"))
         pattern-for-final-with-se3 (re-pattern (str "s\\.*" (cljstr/join "\\.+" (map #(str "\\#{" % "}") (apply concat (repeat 3 nm)))) "\\.*e"))
         N3 (bruteforcewithre sc3 pattern-for-potential3 pattern-for-final-with-se3)]
     (if 
       (= (quot N3 N2) (quot N2 N1)) 
       (* N3 (quot N3 N2) (quot N3 N2) )
       (let [sc5 (str sc3 \? sc2)
             pattern-for-potential5 (re-pattern (str "[\\?|\\.]*" (cljstr/join "[\\?|\\.]+" (map #(str "[\\?|\\#]{" % "}") (apply concat (repeat 5 nm)))) "[\\?|\\.]*"))
             pattern-for-final-with-se5 (re-pattern (str "s\\.*" (cljstr/join "\\.+" (map #(str "\\#{" % "}") (apply concat (repeat 5 nm)))) "\\.*e"))]
         (bruteforcewithre sc5 pattern-for-potential5 pattern-for-final-with-se5)
       ))
     ))
  ([il l]
   (prn il)
   (fromline-bruteforcewithre-factor5 l)))

(defn d12-22h-part1
  [input]
  (let [lines (cljstr/split-lines input)]
    #_(for [l lines]
      (do
        (prn l)
        (let [[sc nm] (d12p1parseinput l)
              pattern-for-potential (re-pattern (str "[\\?|\\.]*" (cljstr/join "[\\?|\\.]+" (map #(str "[\\?|\\#]{" % "}") nm)) "[\\?|\\.]*" ))
              pattern-for-final-with-se (re-pattern (str "s\\.*" (cljstr/join "\\.+" (map #(str "\\#{" % "}") nm)) "\\.*e"))
              ]
          
          (println " " sc)
          (println " " nm)

          (println "   " pattern-for-potential)
          (println "   " pattern-for-final-with-se)

          (println "     " (bruteforcewithre sc pattern-for-potential pattern-for-final-with-se))

          (newline)
          
          )
        
        )
      )
    
    (apply + (map fromline-bruteforcewithre lines))
    ))

(defn d12-22h
  [input]
  (let [lines (cljstr/split-lines input)]
    #_(for [l lines]
      (do
        (prn l)
        (let [[sc nm] (d12p1parseinput l)
              pattern-for-potential (re-pattern (str "[\\?|\\.]*" (cljstr/join "[\\?|\\.]+" (map #(str "[\\?|\\#]{" % "}") nm)) "[\\?|\\.]*"))
              pattern-for-final-with-se (re-pattern (str "s\\.*" (cljstr/join "\\.+" (map #(str "\\#{" % "}") nm)) "\\.*e"))
              sc2 (str sc \? sc)
              pattern-for-potential2 (re-pattern (str "[\\?|\\.]*" (cljstr/join "[\\?|\\.]+" (map #(str "[\\?|\\#]{" % "}") (apply concat (repeat 2 nm)))) "[\\?|\\.]*"))
              pattern-for-final-with-se2 (re-pattern (str "s\\.*" (cljstr/join "\\.+" (map #(str "\\#{" % "}") (apply concat (repeat 2 nm)))) "\\.*e"))
              sc3 (str sc2 \? sc)
              pattern-for-potential3 (re-pattern (str "[\\?|\\.]*" (cljstr/join "[\\?|\\.]+" (map #(str "[\\?|\\#]{" % "}") (apply concat (repeat 3 nm)))) "[\\?|\\.]*"))
              pattern-for-final-with-se3 (re-pattern (str "s\\.*" (cljstr/join "\\.+" (map #(str "\\#{" % "}") (apply concat (repeat 3 nm)))) "\\.*e"))]

          ;; (println " " sc)
          ;; (println " " sc2)
          ;; (println " " sc3)

          (println "     " (bruteforcewithre sc pattern-for-potential pattern-for-final-with-se))
          (println "     " (bruteforcewithre sc2 pattern-for-potential2 pattern-for-final-with-se2))
          (println "     " (bruteforcewithre sc3 pattern-for-potential3 pattern-for-final-with-se3))

          (newline))))

    (apply + (map-indexed fromline-bruteforcewithre-factor5 lines))

  ))

(defn mainD12
  []
  (println "Day 12")
  (println d12sample1)
  ;; (prn (d12part1 d12sample1))
  ;; (prn (d12part1 (slurp "input/day12.txt")))
  (newline)
  (println "part 2")
  (prn (d12-22h d12sample1))
  (prn (d12-22h (slurp "input/day12.txt")))
  ;; (prn (d12part2 d12sample1))
  ;; (prn (d12part2 (slurp "input/day12.txt")))
  )


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [available-days {
    ;; 0 mainD0
    ;;                     1 mainD1
    ;;                     2 mainD2
    ;;                     3 mainD3
    ;;                     4 mainD4
    ;;                     5 mainD5
    ;;                     7 mainD7
    ;;                     8 mainD8
    ;;                     9 mainD9
    ;;                     10 mainD10
    ;;                     11 mainD11
                        12 mainD12}

        this-day (if args (parse-long (first args)) (apply max (keys available-days)))]
    (if
      (contains? available-days this-day)
      ((get available-days this-day))
      ((get available-days (apply max (keys available-days)))))))
