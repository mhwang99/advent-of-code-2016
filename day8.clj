
(defn rect
  [m [y x]]
  (reduce #(assoc %1 %2 \#) m
          (for [i (range x) j (range y)]
            [i j])))

(defn rotr
  [m [i l]]
  (let [l (mod l 50)
        m (reduce #(assoc %1 [i (+ %2 l)] (get %1 [i %2])) m (reverse (range 50)))]
    (reduce #(assoc %1 [i %2] (get %1 [i (+ %2 50)])) m (range l))))

(defn rotc
  [m [j l]]
  (let [l (mod l 6)
        m (reduce #(assoc %1 [(+ %2 l) j] (get %1 [%2 j])) m (reverse (range 6)))]
    (reduce #(assoc %1 [%2 j] (get %1 [(+ %2 6) j])) m (range l))))

(defn parse
  [l]
  (let [p (re-seq #"\d+" l)
        p (map #(Integer. %) p)]
    (cond
      (re-find #"rect" l) [rect p]
      (re-find #"column" l) [rotc p]
      :else [rotr p])))

(defn prt
  [m]
  (let [sc (vec (repeat 6 (vec (repeat 50 \.))))
        sc (reduce-kv #(assoc-in %1 %2 %3) sc m)]
    (doseq [l (map #(apply str %) sc)]
      (println l))))


(defn day8
  [src]
  (let [ps (for [i (range 6) j (range 50)] [i j])
        m (reduce #(assoc %1 %2 \.) {} ps)
        m (reduce #((first %2) %1 (second %2)) m (map parse src))
        m (reduce #(assoc %1 %2 (get m %2)) {} ps)]
    (prt m)
    (reduce #(if (= (second %2) \#) (inc %1) %1) 0 m)))

(day8 (clojure.string/split (slurp "day8.txt") #"\n"))
