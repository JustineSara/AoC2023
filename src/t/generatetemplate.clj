(ns t.generatetemplate
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))


(def template
"(ns t.d<DAYNUM>
  (:gen-class)
  (:require
   [clojure.string :as cljstr]
   [clojure.set :as cljset]))

(def sample \"\")

(defn parse-input
  [input]
  (cljstr/split-lines input)
)


(defn d<DAYNUM>p1
  [input]
  (let [x (parse-input input)
        ]
     x
  ))

(defn d<DAYNUM>p2
  [input]
  (let [x (parse-input input)
        ]
    x
  ))

(defn -main
  [& args] 
  (println \"day<DAYNUM\")
  (println sample)
  (newline)
  
  (println \"part1\")
  (prn (d<DAYNUM>p1 sample))
;;  (prn (d<DAYNUM>p1 (slurp \"input/day<DAYNUM>.txt\")))
  
;;  (newline)
;;  (println \"part2\")
;;  (prn (d<DAYNUM>p2 sample))
;;  (prn (d<DAYNUM>p2 (slurp \"input/day<DAYNUM>.txt\")))
  )
"
  )

(defn -main
  [& args]
  (when-let [d (first args)]
    (let [t (cljstr/replace template "<DAYNUM>" d)]
      (prn (str "d" d ".clj"))
      (if (.exists (clojure.java.io/file (str "src/t/d" d ".clj")))
        (println "The code file for day " d "already exists. Do nothing.")
        (spit (str "src/t/d" d ".clj") t))
      (if (.exists (clojure.java.io/file (str "input/day" d ".txt")))
        (println "The input file for day " d "already exists. Do nothing.")
        (spit (str "input/day" d ".txt") ""))
      (println (str "Created files for day " d " based on template (if not existing)")))))