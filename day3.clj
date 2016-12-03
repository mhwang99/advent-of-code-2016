(def src (map #(Integer. %) (rest (str/split (slurp "day3.txt") #"\s+"))))
(def src-part1 (partition 3 src))
(def src-part2
  (partition 3
             (concat (take-nth 3 src)
                     (take-nth 3 (rest src))
                     (take-nth 3 (drop 2 src)))))

(defn tri
  [l]
  (let [s (apply + l)
        m (apply max l)
        d (- s m)]
    (> d m)))

(defn day3 [l]
  (reduce #(if (tri %2) (inc %1) %1) 0 l))

(day3 src-part1)
(day3 src-part2)
