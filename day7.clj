
(defn abba?
  [s]
  (let [l (loop [s s r []]
            (if (= (count s) 4) (conj r s)
              (recur (rest s) (conj r (take 4 s)))))]
    (loop [s (first l) l (rest l)]
      (if (nil? s) false
        (cond
          (and
            (not= (first s) (second s))
            (= (nth s 1) (nth s 2))
            (= (first s) (last s))) true
          :else (recur (first l) (rest l)))))))


(defn tls?
  [l]
  (let [a (filter #(not= (last %) \]) (re-seq #"\w+]*" l))
        b (map #(drop-last 1 %) (re-seq #"\w+]" l))]
    (and (some abba? a) (not (some abba? b)))))

(defn get-aba
  [s]
  (let [l (loop [s s r []]
            (cond
              (= (count s) 3) (conj r s)
              :else (recur (rest s) (conj r (take 3 s)))))]
    (filter #(and (not= (first %) (second %))
                  (= (first %) (last %))) l)))


(defn ssl?
  [l]
  (let [a (filter #(not= (last %) \]) (re-seq #"\w+]*" l))
        b (map #(drop-last 1 %) (re-seq #"\w+]" l))
        a (reduce #(concat %1 (get-aba %2)) [] a)
        b (reduce #(concat %1 (get-aba %2)) [] b)]
    (some odd?
          (for [x a y b]
            (if (and (= (first x) (second y))
                     (= (second x) (first y)))
              1 0)))))

(defn day7
  [f l]
  (reduce #(if (f %2) (inc %1) %1) 0 l))

;part1
(day7 tls? (clojure.string/split (slurp "day7.txt") #"\s+"))
;part2
(day7 ssl? (clojure.string/split (slurp "day7.txt") #"\s+"))

