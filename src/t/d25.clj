(ns t.d25
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr")



(defn parse-input
  [input]
(->> input
     cljstr/split-lines
     (map (fn [l] (cljstr/split l #":| ")))
     (mapcat (fn [[f _ & r]] (map (fn [rr] #{f rr}) r)) )
     ((fn [x]  [x 
                (reduce #(apply conj %1 %2 ) #{} x)]))
))

(defn one-node-links
  [links n]
  (loop [lks links
         sol #{}]
    (if (empty? lks)
      sol
      (let [[l & lks] lks]
        (if (contains? l n)
          (recur lks (apply conj sol l ))
          (recur lks sol)
          )
        ))))
(def node-links-to (memoize one-node-links))

(defn dist-2-nodes
  [links n1 n2]
  (loop [l [[n1 0]]]
    (let [[[n d] & l] (sort-by second l)]
      (if (= n n2)
        d
        (recur (concat l (map (fn [nn] [nn (inc d)]) (node-links-to links n))))))))

(defn path-2-nodes
  [links n1 n2]
  (loop [l [[n1 0 []]]]
     (let [[[n d pairs] & l] (sort-by second l)]
       (if (= n n2)
         pairs
         (recur (concat l (map (fn [nn] [nn (inc d) (conj pairs {n nn})]) (node-links-to links n))))))   
    ))

(defn links-to-cut
  [links nodes]
  (->>
    (for [n1 nodes n2 nodes]
      (path-2-nodes links n1 n2)
    )
    (apply concat)
   (group-by identity)
   (map (fn [[k v]] [k (count v)]))
   (into {})
   (sort-by second >)
  ;;  (map first)
   )
  )

(defn links-to-cut-2
  [links nodes]
  (->>
   (loop [ns (rest nodes)
          n (first nodes)
          ns2 (rest nodes)
          links-npath (into {} (map (fn [l] [l 0]) links))]
     (cond 
       (empty? ns) links-npath
       (empty? ns2) (do (print "|") (recur (rest ns) (first ns) (rest ns) links-npath))
       :else
       (let [[n2 & ns2] ns2
             ls (path-2-nodes links n n2)]
        ;;  (prn ls)
         (recur ns n ns2 (reduce #(assoc %1 (apply set %2) (inc (get %1 (apply set %2)))) links-npath ls))
         )
       )
     )
   (sort-by second >)
  ;;  (map first)
   ))


(defn find-groups
  [links nodes]
  (loop [left-nodes (set (rest nodes))
         groups []
         cur-nodes [(first nodes)]
         cur-group #{(first nodes)}]
    (cond
      (empty? left-nodes) (conj groups cur-group)
      (empty? cur-nodes) (let [n (first left-nodes)] 
                           (recur (disj left-nodes n) (conj groups cur-group) [n] #{n}))
      :else
      (let [[n & cur-nodes] cur-nodes
            next-nodes (->> n
                            (node-links-to links)
                            (filter (fn [nn] (not (contains? cur-group nn)))))]
        (recur (apply disj left-nodes next-nodes) groups (concat cur-nodes next-nodes) (apply conj cur-group next-nodes))))))

(defn bruteforce
  [links nodes]
  (let [set-links (set links)]
  (loop [l1 (nth links 0)
         l1s (nthrest links 1)
         l2 (nth links 1)
         l2s (nthrest links 2)
         l3 (nth links 2)
         l3s (nthrest links 3)
         iter 0]
    (when (zero? (mod iter 10000)) (print "|"))
    (let [gps (find-groups (disj set-links l1 l2 l3) nodes)]
      (if (= 2 (count gps)) 
        (* (count (first gps)) (count (second gps)))
        (if (empty? l3s)
          (if (empty? l2s)
            (if (= 2 (count l1s))
              (println "ooups found nothing")
              (recur (nth l1s 0) (nthrest l1s 1)
                     (nth l1s 1) (nthrest l1s 2)
                     (nth l1s 2) (nthrest l1s 3)
                     (inc iter)))
            (recur l1 l1s (first l2s) (rest l2s) (second l2s) (nthrest l2s 2) (inc iter)))
          (recur l1 l1s l2 l2s (first l3s) (rest l3s) (inc iter)))
      )
    ))
    ))


(defn one-link-steps-everyth
  [l links nodes]
  (loop [left-nodes (apply disj nodes l)
         cur-nodes [(first l) (second l)]
         d 0]
    (if (empty? left-nodes)
      d
      (let [new-nodes (->> cur-nodes
                           (map #(node-links-to links %))
                           (apply cljset/union)
                           (filter #(contains? left-nodes %)))]
        (recur (apply disj left-nodes new-nodes) new-nodes (inc d))
        ))
    ))

(defn links-nsteps-allvisited
  [links nodes]
  (->> links
       (map (fn [l] [l (one-link-steps-everyth l links nodes)]))
       (sort-by second)
       )
  )


(defn bruteforce-2
  [links nodes tbc-links]
  (println "number of links to be combined:" (count tbc-links))
  (println "number of combo:" (- (* (count tbc-links) (count tbc-links) (count tbc-links)) (* (count tbc-links) (count tbc-links)) (count tbc-links)))
  (let [set-links (set links)]
    (loop [l1 (nth tbc-links 0)
           l1s (nthrest tbc-links 1)
           l2 (nth tbc-links 1)
           l2s (nthrest tbc-links 2)
           l3 (nth tbc-links 2)
           l3s (nthrest tbc-links 3)
           iter 0]
      (when (zero? (mod iter 10000)) (print "|"))
      (let [gps (find-groups (disj set-links l1 l2 l3) nodes)]
        (if (= 2 (count gps))
          (* (count (first gps)) (count (second gps)))
          (if (empty? l3s)
            (if (empty? l2s)
              (if (= 2 (count l1s))
                (println "ooups found nothing")
                (recur (nth l1s 0) (nthrest l1s 1)
                       (nth l1s 1) (nthrest l1s 2)
                       (nth l1s 2) (nthrest l1s 3)
                       (inc iter)))
              (recur l1 l1s (first l2s) (rest l2s) (second l2s) (nthrest l2s 2) (inc iter)))
            (recur l1 l1s l2 l2s (first l3s) (rest l3s) (inc iter))))))))

(defn split-in-2
  [links nodes]
  (let [n1 (first nodes)
        n2 (rand-nth (rest nodes))]
  (loop [n-grp (into {} (map (fn [n] [n (cond (= n n1) 1 (= n n2) -1 :else 0)]) nodes))]
    (prn n-grp)
    (if (some #(zero? (second %)) n-grp)
      (recur (->> nodes
                 (map (fn [n] [n (map (fn [nn] (get n-grp nn)) (node-links-to links n))]))
                 (map (fn [[n nns]] [n (/ (apply + nns) (count nns))]))
                 ))
      [n-grp 
       (->> n-grp 
            (group-by second)
            (map (fn [[g ln]] [g (map first ln)]))
            (into {})
       )
            ]
      
      )
    ))
  )

(defn d25p1
  [input]
  (let [[links nodes] (parse-input input)
        _ (println "number of nodes : " (count nodes))
        _ (println "number of links : " (count links))
        ;; n (first nodes)
        ;; _ (println "number of pairs of nodes : " (* (count nodes) (dec (count nodes))))
        ;; links-npath (links-to-cut-2 links nodes)
        ;; l1 (first links-npath)
        ;; _ (println "cut " l1)
        ;; l2 (second links-npath)
        ;; _ (println "cut " l2)
        ;; l3 (nth links-npath 2)
        ;; _ (println "cut " l3)
        ;; gps (find-groups (apply disj (set links) (map first [l1 l2 l3])) nodes)
        links-dist (links-nsteps-allvisited links nodes)
        md (apply min (map second links-dist))
        ]
    #_(for [n nodes]
        (let [con (one-node-links links n)]
          (println "  for node " n ":" (count con) "connexions")))
    #_(for [n1 nodes
            n2 nodes]
        (println "distance " n1 "=" n2 ":" (dist-2-nodes links n1 n2)))
    #_(for [n1 nodes
            n2 nodes]
        (println "path " n1 "-" n2 ":" (path-2-nodes links n1 n2)))
    #_(find-groups links nodes)
    #_(find-groups (disj (set links) #{"hfx" "pzl"} #{"bvb" "cmg"} #{"nvd" "jqt"}) nodes)


    ;; (println links)
    ;; (prn (map #(apply set (first %)) [l1 l2 l3]))
    ;; (println (apply disj (set links) (map #( set (first %)) [l1 l2 l3])))
    ;; (println "groups" gps)

    ;; (println (time (links-to-cut links nodes)))
    ;; (println (time (links-to-cut-2 links nodes)))

    ;; (println "solution: ")
    ;; (apply * (map count gps))

    ;; (println "number of iter : " (- (* (count links) (count links) (count links)) (* (count links) (count links)) (count links)))
    ;; (time (bruteforce links nodes))

    ;; (prn nodes)
    ;; (prn links)

    (->> links-dist
         (filter (fn [[_ d]] (= d md)))
         (map first)
         (bruteforce-2 links nodes)
         )

    ))


(defn d25p2
  [input]
  (let [x (parse-input input)
        ]
    x
  ))

(defn -main
  [& args] 
  (println "day25")
  (println sample)
  (newline)
  
  (println "part1")
  (prn (d25p1 sample))
 (prn (d25p1 (slurp "input/day25.txt")))
  
;;  (newline)
;;  (println "part2")
;;  (prn (d25p2 sample))
;;  (prn (d25p2 (slurp "input/day25.txt")))
  )
