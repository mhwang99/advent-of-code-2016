(defn getmap
  [m e]
  (loop [i 0 e e m m]
    (if (empty? e) m
      (let [f (first e)]
        (if (get-in m [i f])
          (recur (inc i) (rest e) (update-in m [i f] inc))
          (recur (inc i) (rest e) (assoc-in m [i f] 1)))))))

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

