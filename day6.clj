(defn getmap
  [m s]
  (loop [i 0 s s m m]
    (if (empty? s) m
      (let [c (first s)]
        (if (get-in m [i c])
          (recur (inc i) (rest s) (update-in m [i c] inc))
          (recur (inc i) (rest s) (assoc-in m [i c] 1)))))))

(defn getone
  [f m]
  (map #(->> %
             last
             (sort-by last)
             f
             first)
       (sort-by first m)))


(defn day6
  [f l]
  (apply str (getone f (reduce #(getmap %1 %2) {} l))))

;part 1
(day6 last (clojure.string/split (slurp "day6.txt") #"\s+"))

;part 2
(day6 first (clojure.string/split (slurp "day6.txt") #"\s+"))

